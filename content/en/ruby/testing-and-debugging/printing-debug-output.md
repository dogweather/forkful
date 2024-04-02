---
date: 2024-01-20 17:53:10.068881-07:00
description: "Printing debug output in Ruby is like leaving breadcrumb trails in your\
  \ code to track variable values and program flow. Programmers do it to catch bugs\
  \ by\u2026"
lastmod: '2024-03-13T22:45:00.553308-06:00'
model: gpt-4-1106-preview
summary: "Printing debug output in Ruby is like leaving breadcrumb trails in your\
  \ code to track variable values and program flow. Programmers do it to catch bugs\
  \ by\u2026"
title: Printing debug output
weight: 33
---

## What & Why?
Printing debug output in Ruby is like leaving breadcrumb trails in your code to track variable values and program flow. Programmers do it to catch bugs by checking what their code’s up to at various points.

## How to:
In Ruby, `puts` and `p` are your go-to methods for quick output to the console. 

```Ruby
def who_said_what
  quote = "To be or not to be"
  author = "Shakespeare"
  puts "Quote: #{quote}"
  p "Said by: #{author}"
end

who_said_what
```

Sample output:

```
Quote: To be or not to be
"Said by: Shakespeare"
```

The `puts` method prints a human-readable output, adding a new line at the end. In contrast, `p` prints the value in a more raw form, useful when you need to see if something's a string or not.

## Deep Dive
Back before fancy IDEs, printing to the console was debugging. It’s an old but gold technique, especially when you want to avoid the overhead of setting up a debugger. 

As alternatives, you can use `pp` for pretty-printing complex objects, or gem libraries like `awesome_print` for enhanced readability. If your debug output is getting too chatty, consider a logging library to control levels of verbosity.

Implementation-wise, `puts` and `p` write to `$stdout`, a global I/O stream in Ruby. Output can be redirected if needed. Remember, while these methods are convenient, excessive debug prints can clutter your console and make debugging harder.

## See Also
- Ruby documentation for `Kernel#puts`: https://ruby-doc.org/core/Kernel.html#method-i-puts
- Ruby documentation for `Kernel#p`: https://ruby-doc.org/core/Kernel.html#method-i-p
- A guide to pretty printing in Ruby: https://ruby-doc.org/stdlib/libdoc/pp/rdoc/PP.html
- The Awesome Print gem for fancy output: https://rubygems.org/gems/awesome_print/
