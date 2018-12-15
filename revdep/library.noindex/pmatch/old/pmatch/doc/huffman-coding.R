## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(pmatch)

Tree := Leaf(char : character, count : integer) | 
        Node(count : integer, left : Tree, right : Tree)
Bit := L | R

## ------------------------------------------------------------------------
llist := NIL | CONS(car, cdr : llist)

## ------------------------------------------------------------------------
single <- function(val) CONS(val, NIL)
pair := PAIR(x, y)

## ------------------------------------------------------------------------
llength <- function(llist, acc = 0) {
    cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llength(cdr, acc + 1)
    )
}

llrev <- function(llist, acc = NIL) {
    cases(
        llist,
        NIL -> acc,
        CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
    )
}

lltake <- function(llist, k, acc = NIL) {
    if (k == 0) return(llrev(acc))
    cases(
        llist,
        NIL -> stop("There were less than k elements in the list"),
        CONS(car, cdr) -> lltake(cdr, k - 1, CONS(car, acc))
    )
}
lldrop <- function(llist, k, acc = NIL) {
    if (k == 0) return(llist)
    cases(
        llist,
        NIL -> stop("There were less than k elements in the list"),
        CONS(car, cdr) -> lldrop(cdr, k - 1)
    )
}

## ------------------------------------------------------------------------
merge_sort <- function(merge, xs) {
    n <- llength(xs)
    if (n < 2) return(xs)
    half <- n %/% 2 
    first <- lltake(xs, half)
    second <- lldrop(xs, half)
    merge(merge_sort(merge, first), merge_sort(merge, second))
}

## ------------------------------------------------------------------------
alpha_merge <- function(xs, ys) {
    cases(
        ..(xs, ys),
        ..(xs, NIL) -> xs,
        ..(NIL, ys) -> ys,
        ..(CONS(PAIR(p,n), xs), CONS(PAIR(q,m), ys)) -> {
            if (p == q) {
                CONS(PAIR(p,n+m), alpha_merge(xs, ys))
            } else if (p < q) {
                CONS(PAIR(p,n), alpha_merge(xs, CONS(PAIR(q,m), ys)))
            } else {
                CONS(PAIR(q,m), alpha_merge(CONS(PAIR(p,n), xs), ys))
            }
        }
    )
}

## ------------------------------------------------------------------------
llist_from_list <- function(x) {
    llist <- NIL
    n <- length(x)
    while (n > 0) {
        llist <- CONS(x[[n]], llist)
        n <- n - 1
    }
    llist
}

## ------------------------------------------------------------------------
xs <- llist_from_list(list(
    PAIR("a", 1), PAIR("b", 2)
))
ys <- llist_from_list(list(
    PAIR("a", 3), PAIR("c", 3)
))
alpha_merge(xs, ys)

## ------------------------------------------------------------------------
llconcat <- function(xs, ys) {
    cases(xs, 
          NIL -> ys,
          CONS(x,xs) -> CONS(x, llconcat(xs, ys)))
}

## ------------------------------------------------------------------------
zs <- llconcat(xs, ys)
merge_sort(alpha_merge, zs)
merge_sort(alpha_merge, llrev(zs))

## ------------------------------------------------------------------------
freq_merge <- function(xs, ys) {
    cases(
        ..(xs, ys),
        ..(xs, NIL) -> xs,
        ..(NIL, ys) -> ys,
        ..(CONS(PAIR(p,n), xs), CONS(PAIR(q,m), ys)) -> {
            if (n < m || (n == m && p < q)) {
                CONS(PAIR(p,n), freq_merge(xs, CONS(PAIR(q,m), ys)))
            } else {
                CONS(PAIR(q,m), freq_merge(CONS(PAIR(p,n), xs), ys))
            }      
        }
    )
}

## ------------------------------------------------------------------------
xs <- llist_from_list(list(
    PAIR("a", 5), PAIR("b", 3), PAIR("c", 2)
))
merge_sort(freq_merge, xs)

## ------------------------------------------------------------------------
llmap <- function(llist, f, acc = NIL) {
    cases(
        llist,
        NIL -> llrev(acc),
        CONS(car, cdr) -> llmap(cdr, f, CONS(f(car), acc))
    )
}

## ------------------------------------------------------------------------
string_to_list <- function(x)
    llist_from_list(strsplit(x, "")[[1]])

string_to_list("foobar")

## ------------------------------------------------------------------------
library(magrittr)
frequency = . %>% 
    string_to_list() %>% 
    llmap(function(char) PAIR(char, 1L)) %>%
    merge_sort(alpha_merge, .) %>%
    merge_sort(freq_merge, .)

## ------------------------------------------------------------------------
frequency("foo")

## ------------------------------------------------------------------------
to_tree_list <- . %>% llmap(
    function(x) cases(x, PAIR(char,count) -> Leaf(char, count))
)
xs <- "foo" %>% string_to_list() %>% llmap(function(char) PAIR(char, 1L))
to_tree_list(xs)

## ------------------------------------------------------------------------
make_codes <- function(ts) {
    cases(
        ts,
        CONS(t, NIL) -> t,
        ts -> make_codes(amalgamate(ts))
    )
}

## ------------------------------------------------------------------------
amalgamate <- function(ts) {
    cases(
        ts,
        CONS(t1, CONS(t2, ts)) -> ins_tree(pair(t1, t2), ts)
    )
}

## ------------------------------------------------------------------------
value <- function(tree) {
    cases(
        tree,
        Leaf(., n) -> n,
        Node(n, ., .) -> n
    )
}
pair <- function(t1, t2) Node(value(t1) + value(t2), t1, t2)

## ------------------------------------------------------------------------
ins_tree <- function(t, ts) {
    cases(
        ..(t, ts),
        ..(t, NIL) -> single(t),
        ..(t1, CONS(t2, rest)) -> {
            v1 <- value(t1)
            v2 <- value(t2)
            if (v1 < v2) {
                CONS(t1, CONS(t2, rest))
            } else {
                CONS(t2, ins_tree(t1, rest))
            }
        }
    )
}

## ------------------------------------------------------------------------
make_tree <- . %>%
    to_tree_list() %>%
    make_codes()

## ------------------------------------------------------------------------
tree <- "foo" %>% frequency() %>% make_tree()
tree

## ---- echo=FALSE---------------------------------------------------------
library(tidygraph)
library(ggraph)

plot_tree <- function(tree) {
    dfn <- function(tree, n) {
        cases(
            tree,
            Leaf(char,count) ->
                list(structure(Leaf(char,count), number = n), n + 1L),
            Node(count, left, right) -> {
                bind[left,n] <- dfn(left, n)
                bind[right,n] <- dfn(right, n)
                list(structure(Node(count, left, right), number = n), n + 1L)
            }
        )
    }
    bind[tree,size] <- dfn(tree, 0L)
    
    node_label <- rep("", size)
    
    from <- vector("integer", length = size - 1)
    to <-  vector("integer", length = size - 1)
    edge_label <- vector("character", length = size - 1)
    
    index <- 1
    populate_table <- function(tree) {
        cases(
            tree,
            Leaf(char, count) ~ {
                node_label[attr(tree, "number") + 1] <<- char
            },
            Node(., left, right) ~ {
                this_node_n <- attr(tree, "number")
                left_node_n <- attr(left, "number")
                right_node_n <- attr(right, "number")
                from[index] <<- this_node_n ; to[index] <<- left_node_n
                edge_label[index] <<- "L"
                index <<- index + 1
                from[index] <<- this_node_n ; to[index] <<- right_node_n
                edge_label[index] <<- "R"
                
                index <<- index + 1
                populate_table(left)
                populate_table(right)
            }
        )
    }
    populate_table(tree)
    
    nodes <- data.frame(node_label = node_label, 
                        stringsAsFactors = FALSE)
    # add one because ggraph doesn't like zero
    edges <- data.frame(from = from + 1, to = to + 1, 
                        edge_label = edge_label,
                        stringsAsFactors = FALSE)
    tidygraph::tbl_graph(nodes, edges) %>%
        ggraph(layout = "tree") +
        geom_node_point(aes(filter = (node_label == "")), size = 5) +
        geom_node_text(aes(filter = (node_label != ""), label = node_label)) +
        geom_edge_link(aes(label = edge_label, hjust = 2),
                       arrow = arrow(length = unit(4, 'mm')), 
                       end_cap = circle(3, 'mm')) +
        theme_graph()
}

## ------------------------------------------------------------------------
"foobarbaz" %>% frequency() %>% make_tree() %>% plot_tree()

## ------------------------------------------------------------------------
as.list.llist <- function(x, all.names = FALSE, sorted = FALSE, ...) {
    n <- llength(x)
    v <- vector("list", length = n)
    i <- 1
    while (i <= n) {
        v[i] <- x$car
        i <- i + 1
        x <- x$cdr
    }
    v
}
as.vector.llist <- function(x, mode = "any") {
    unlist(as.list(x))
}

toString.code <- function(x, ...) {
    x %>% llmap(toString) %>% as.vector() %>% paste0(collapse = "")
}

code <- llist_from_list(
    list(L, L, R, L)
)
code
class(code) <- c("code", class(code))
code %>% toString() %>% cat()

## ------------------------------------------------------------------------
convert <- function(code, tree) {
    cases(
        tree,
        Leaf(char, count) -> single(
            PAIR(char,structure(code, class = c("code", class(code))))
        ),
        Node(count, t1, t2) -> llconcat(
            convert(llconcat(code, single(L)), t1),
            convert(llconcat(code, single(R)), t2)
        )
    )
}

code_table <- . %>% convert(NIL, .) %>% 
    structure(class = c("code_table", class(.)))

## ------------------------------------------------------------------------
toString.code_table <- function(x, ...) {
    x %>% llmap(function(p) 
        cases(p, PAIR(x,y) ~ paste0(x, " => ", toString(y)))
    ) %>% as.vector() %>% paste0(collapse = "\n")
}

## ------------------------------------------------------------------------
tree <- "foobaar" %>% frequency() %>% make_tree()
table <- tree %>% code_table()
tree %>% plot_tree()
table %>% toString() %>% cat()

tree <- "foobarbaz" %>% frequency() %>% make_tree()
table <- tree %>% code_table()
tree %>% plot_tree()
table %>% toString() %>% cat()

## ------------------------------------------------------------------------
codes <- . %>% frequency() %>% make_tree()

## ------------------------------------------------------------------------
tree <- codes("foobarbaz")
tree %>% code_table() %>% toString() %>% cat()

## ------------------------------------------------------------------------
lookup_table <- function(table, char) {
   cases(
       table,
       NIL -> stop(paste(char, "was not found in the table")),
       CONS(PAIR(ch,n), tb) -> {
           if (ch == char) {
               n
           } else {
               lookup_table(tb, char)
           }
       }
   ) 
}

## ------------------------------------------------------------------------
llreduce <- function(llist, f, acc) {
    cases(
        llist,
        NIL -> acc,
        CONS(val, rest) -> llreduce(rest, f, f(acc, val))  
    )
}

## ------------------------------------------------------------------------
code_message <- function(message, table) {
    message %>% string_to_list() %>%
        llmap(function(char) lookup_table(table, char)) %>%
        llreduce(llconcat, NIL) %>% 
        structure(class = c("code", class(.)))
}

tree <- codes("foobarbaz")
table <- tree %>% code_table()
code <- "foobar" %>% code_message(table)
code %>% toString() %>% cat()

## ------------------------------------------------------------------------
decode_message <- function(tree, code) {
    decode_by_tree <- function(tr, code) {
        cases(
            ..(tr, code),
            ..(Node(n, t1, t2), CONS(L, rest)) -> decode_by_tree(t1, rest),
            ..(Node(n, t1, t2), CONS(R, rest)) -> decode_by_tree(t2, rest),
            ..(Leaf(char, n), rest) -> CONS(char, decode_by_tree(tree, rest)),
            ..(t, NIL) -> NIL,
        )
    }
    decode_by_tree(tree, code) %>% as.vector() %>% paste0(collapse = "")
}

decode_message(tree, code)

