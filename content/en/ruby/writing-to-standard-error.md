---
title:    "Ruby recipe: Writing to standard error"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered the need to communicate errors or important information to the user while your program is running. Printing this information to the standard output may not always be the best option as it can get mixed in with other program output. This is where writing to standard error comes in handy.

## How To

To write to standard error in Ruby, you can use the `STDERR.puts` method. This takes in a string or variable and prints it to the standard error stream. Let's take a look at an example:

```Ruby
name = "John"

STDERR.puts "Hello, #{name}! There seems to be an error."
```

The code above will print the string "Hello, John! There seems to be an error." to the standard error. You can also use `STDERR.print` which works in the same way as `puts` but does not add a line break at the end.

```Ruby
STDERR.print "Oops, something went wrong."
```

This will print "Oops, something went wrong." to the standard error without a line break.

Now, let's see what the output looks like:

```
Hello, John! There seems to be an error.
Oops, something went wrong.
```

As you can see, the output from writing to standard error is separated from the standard output, making it easier to identify and debug errors.

## Deep Dive

In Ruby, writing to standard error is essential for handling errors in your programs. You can use the standard error stream to print out error messages or other important information when an exception occurs.

One thing to note is that standard error will print out even if the standard output is being redirected to a file or a different location. This ensures that important information is not missed in case of any errors.

You can also use `STDERR.puts` or `STDERR.print` in combination with `rescue` statements to handle exceptions and print out relevant messages. This can help with troubleshooting and identifying the cause of errors in your code.

## See Also

- [Ruby official documentation on STDERR](https://ruby-doc.org/core-2.5.1/IO.html#method-c-new-label-Standard+Error)
- [Understanding Standard Streams in Ruby](https://dev.to/sublimegeek/ruby-standard-streams-41m8)
- [Handling Errors and Exceptions in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-errors-and-exceptions/cheatsheet)