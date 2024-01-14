---
title:    "Haskell recipe: Writing to standard error"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as stderr, is an important skill for any Haskell programmer to have. It allows for better error handling and debugging in your code. In this blog post, we will explore the reasons behind writing to standard error and how to implement it in your Haskell programs.

## How To

To write to standard error in Haskell, we first need to import the ```System.IO``` library. This library provides functions for working with input and output handles. We will also need to use the ```hPutStrLn``` function, which takes two arguments: the output handle and the string we want to write. Let's look at an example:

```
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
```

When we run this program, we will see the following output in our terminal:

```
This is an error message.
```

Notice that the output is displayed in red, which is the default color for stderr. This makes it easy to distinguish between regular output and error output.

We can also use the ```stderr``` function from the ```System.IO``` library to directly access the stderr handle and write to it. Let's see an example of this:

```
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "This is an error message."
  stderr "This is another error message."
```

In this example, we are using the ```stderr``` function to directly write to the stderr handle. This can be useful when we want to write multiple error messages in a row.

## Deep Dive

Writing to standard error is helpful for a few reasons. First, it allows us to clearly separate our regular output from our error messages. This can make it easier to read and understand the output of our programs.

Additionally, writing to standard error can help with debugging. When we include specific error messages, we can pinpoint exactly where an error occurred in our code. This can save us time and effort when trying to fix bugs.

It's also worth noting that both ```stdout``` (standard output) and ```stderr``` are streams, meaning they both follow the same rules for input/output. However, stderr is specifically designed for error messages, so it's best to use it for that purpose rather than using stdout for both regular and error output.

## See Also

- [Haskell I/O Functions](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- [Writing to Standard Output - Or Trying To](https://www.fpcomplete.com/blog/2016/11/writing-to-standard-output/)
- [Understanding IO](https://en.wikibooks.org/wiki/Haskell/Understanding_IO)