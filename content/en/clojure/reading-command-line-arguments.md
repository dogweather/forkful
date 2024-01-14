---
title:                "Clojure recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

As a beginner in Clojure programming, you may wonder why it is important to know how to read command line arguments. Well, the ability to read and handle command line arguments is crucial for creating versatile and interactive programs. Think of it as the equivalent of a user interface for command line applications. Without it, your program may be limited in its functionality and user-friendliness. So if you want to take your Clojure programming skills to the next level, learning how to read command line arguments is a must!

## How To

Reading command line arguments in Clojure is a straightforward process. First, you need to declare the `cmd-args` parameter in your `fn` as shown below:

```Clojure
(defn my-program [cmd-args] 
	...
)

```

Then, you can use the `slurp` function to read the command line arguments into a variable, for example `input`:

```Clojure
(defn my-program [cmd-args] 
	(let [input (slurp cmd-args)])
	...
)

```

Now, you can use `println` to display the arguments or perform any other necessary operations. Let's say you run your program with the command line arguments `1 2 3`, the output would be:

```Clojure
1
2
3
```

## Deep Dive

The `slurp` function not only reads the command line arguments, but it also converts them into a String type. This can be limiting when you want to work with different data types. To overcome this, you can use the `clojure.edn` library to convert the arguments into Clojure data structures. Here's an example:

```Clojure
(require '[clojure.edn :as edn])

(defn my-program [cmd-args] 
	(let [input (-> cmd-args 
               slurp 
               edn/read-string
           )]
	...
)

```

Now, if you run your program with the command line arguments `[1 2 3]`, the output would be:

```Clojure
[1 2 3]
```

## See Also

To further enhance your Clojure programming skills, check out these resources:

- Official Clojure docs on reading command line arguments: https://clojure.org/reference/other_functions#_command_line_args
- A tutorial on reading command line arguments in Clojure: https://oli.me.uk/2013/06/09/easy-command-line-arguments-for-clojure/
- Clojure cheat sheet for a quick reference: https://clojure.org/api/cheatsheet