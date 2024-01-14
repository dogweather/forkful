---
title:    "Ruby recipe: Writing to standard error"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

As developers, we spend a lot of time debugging and troubleshooting our code. One common issue we often encounter is not being able to see the exact error message when something goes wrong. That's where writing to standard error comes in.

## How To

To write to standard error in Ruby, we use the `STDERR` constant and the `puts` method. Let's take a look at an example:

```Ruby
STDERR.puts "Oops! Something went wrong."
```

The `STDERR` constant represents the standard error output stream, which is separate from the standard output stream used by `puts` and `print`. By using `STDERR.puts`, we can print our error message to the standard error output stream instead of the standard output stream.

Now, let's see what the output looks like when we run our code:

```
Oops! Something went wrong.
```

As you can see, our error message is now printed to the console, separate from any other output that may be present.

## Deep Dive

In Ruby, `puts` actually stands for "put string". This method converts any arguments given to it into strings and then adds a new line character at the end. However, `STDERR.puts` does not add a new line character, meaning we have more control over how our error message is displayed. We can also use `STDERR.print` if we don't want the new line character to be added.

Another thing to keep in mind is that `puts` and `print` both return `nil`, meaning they don't have any meaningful value. This is important to note when using them in conjunction with other methods.

## See Also

- [Ruby Documentation for STDERR](https://ruby-doc.org/core-2.7.0/STDERR.html)
- [Difference between puts and print in Ruby](https://www.rubyguides.com/2019/07/ruby-puts-vs-print/)
- [Ruby Kernel Module](https://ruby-doc.org/core-2.7.0/Kernel.html)