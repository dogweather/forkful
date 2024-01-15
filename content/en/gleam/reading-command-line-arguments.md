---
title:                "Reading command line arguments"
html_title:           "Gleam recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

Command line arguments are an essential aspect of programming, and understanding how to read them can greatly improve your efficiency and productivity as a programmer. In this article, we will delve into Gleam's methods for reading command line arguments, so you can confidently utilize them in your projects.

## How To

Reading command line arguments in Gleam is a simple process that can be broken down into three steps:

1. First, we need to import the `gleam/io` package to access all the necessary functions. We can do this by adding the line `import gleam/io` to the top of our code.

2. Next, we define a `main` function that will serve as the entry point of our code. Inside this function, we use the `gleam/io/args` function to retrieve the command line arguments. We can assign these arguments to a variable using the `let` keyword. 

3. Finally, we can use the `gleam/io/stdout` function to print out the arguments to the terminal. This will allow us to see the arguments in action.

Let's take a look at a basic example of reading command line arguments in Gleam:

```
import gleam/io

fn main() {
  let args = io.args()
  io.stdout(args)
}
```

If we were to run this code with the command `Gleam hello world`, the output would be `["hello", "world"]`, which are the two arguments we passed in.

## Deep Dive

Now that we have a basic understanding of how to read command line arguments in Gleam, let's dive deeper into the topic.

Firstly, it's important to note that command line arguments are separated by spaces. This means that if we wanted to pass in a phrase as an argument, we would need to surround it with quotes. For example, `Gleam "hello world"` would pass in one argument of `"hello world"`.

Additionally, Gleam offers various functions that allow us to manipulate and format the arguments in different ways. Some of these functions include `join`, `split`, and `map`.

It's also worth mentioning that Gleam allows for optional arguments, which can be specified with a flag. These are typically preceded by a `--` and are used to perform specific actions or customize the program's behavior.

Overall, understanding how to read command line arguments in Gleam is a valuable skill that can greatly enhance your programming abilities.

See Also

- [Gleam Documentation for command line arguments](https://gleam.run/book/intro.html#command-line-arguments)
- [Gleam CLI repository](https://github.com/gleam-lang/gleam)