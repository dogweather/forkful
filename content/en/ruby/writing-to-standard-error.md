---
title:                "Writing to standard error"
html_title:           "Ruby recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Ruby is a way for programmers to display error messages or debugging information to the user. Unlike standard output (usually displayed on the terminal), standard error is meant for displaying important information that the user should be aware of, such as unexpected errors or warnings. By using standard error, programmers can ensure that these messages are not overlooked and can be easily identified by the user.

## How to:

To write to standard error in Ruby, you can use the `STDERR.puts` method, followed by the message you want to display. For example:

```Ruby
STDERR.puts "Something went wrong"
```

This will print "Something went wrong" to the standard error stream.

You can also use string interpolation to provide more specific information in your error message. For instance:

```Ruby
username = "John"
STDERR.puts "#{username} is not a valid username"
```

This will output "John is not a valid username" to the standard error stream.

## Deep Dive:

The concept of standard error in programming dates back to the early days of computer programming. It was initially implemented as a way to differentiate between regular output and error messages, as both were printed on the same screen. Nowadays, standard error also serves as a way for programmers to redirect specific types of information to a different location, such as a log file.

In Ruby, an alternative method to writing to standard error is using the `Kernel#warn` method. This method also outputs the given message to the standard error stream, but it includes additional information such as the location of the code where the message was called.

Standard error is implemented in Ruby using the `IO` class, which represents input/output streams. The standard error stream, specifically, is represented by the `STDERR` constant.

## See Also:

To learn more about standard error and its implementation in Ruby, you can refer to the [Ruby documentation for STDERR](https://ruby-doc.org/core-2.7.0/STDERR.html). Additionally, to understand more about the `Kernel#warn` method, you can check out its documentation [here](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-warn).