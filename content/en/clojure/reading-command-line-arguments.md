---
title:    "Clojure recipe: Reading command line arguments"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

As a beginner in Clojure, understanding how to read command line arguments is an essential skill for developing command line applications. Knowing how to access and manipulate these arguments can greatly improve the functionality and flexibility of your programs.

## How To 

To start off, let's take a look at a basic Clojure program that reads command line arguments and prints them out. Open up your preferred command line interface and type in the following commands:

```
Clojure -M -e "(println *command-line-args*)"
```

Press enter and you should see an output that looks something like this:

```
["-M" "-eval" "(println *command-line-args*)"]
```

What this does is it prints out the arguments that were passed in when running the program. In this case, we passed in the -M flag which tells Clojure to load modules before executing the program. This is why we see it as the first argument followed by the -eval flag and the code to be evaluated.

But what if we want to do more than just print out the arguments? We can use the "get" function to access a specific argument at a particular index. Let's take a look at an example:

```
Clojure -M -e "(println (get *command-line-args* 2))"
```

Here, we are telling Clojure to print out the argument at index 2, which in this case is the code to be evaluated. The output should be:

```
"(println *command-line-args*)"
```

Now, let's dive deeper into reading command line arguments.

## Deep Dive

In addition to using the "get" function, we can also use the "nth" function to access a specific argument at an index. This is helpful when you have a larger number of arguments and need to access a specific one without counting them manually.

```
Clojure -M -e "(println (nth *command-line-args* 1))"
```

In this example, we are printing out the argument at index 1, which in this case is the -eval flag. The output should be:

```
"-eval"
```

Another useful function for reading command line arguments is the "subs" function. This allows us to extract a substring from a larger string, which can be helpful if our arguments include words or phrases. Let's take a look:

```
Clojure -M -e "(println (subs (nth *command-line-args* 0) 1 3))"
```

Here, we are printing out a substring of the first argument, starting at index 1 and ending at index 3. The output should be:

```
"-M"
```

By utilizing these functions, we can easily access and manipulate command line arguments to improve the functionality of our programs.

## See Also

To learn more about Clojure and its functions, check out these helpful resources:

- [Official Clojure website](https://clojure.org)
- [Clojure cheat sheet](https://clojure.org/api/cheatsheet)
- [Clojure for the Brave and True](https://www.braveclojure.com)

Happy coding!