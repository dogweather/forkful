---
title:                "Writing to standard error"
date:                  2024-01-19
simple_title:         "Writing to standard error"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (`stderr`) is a stream separate from standard output (`stdout`) used mainly for outputting error messages or diagnostics. Programmers use it to keep error messages from mixing with regular program output, which helps in both debugging and output processing.

## How to:
In Ruby, you can write to standard error using `$stderr.puts` or its shorthand `STDERR.puts`. Here's a quick example:

```ruby
puts "This will go to standard output."
$stderr.puts "This will go to standard error."

# Shorthand version:
STDERR.puts "This will also go to standard error."
```

Open your terminal, run the script, and notice how everything still appears together by default. Redirecting is needed to separate the streams. Here's how you can do that:

```shell
ruby your_script.rb >output.txt 2>error.txt
```

This command redirects standard output to `output.txt` and standard error to `error.txt`.

## Deep Dive
The concept of `stderr` goes back to Unix's earliest days. It's designed for error messages, ensuring they are visible even when `stdout` is redirected. While `$stderr.puts` and `STDERR.puts` are common in Ruby, there are other ways to write to `stderr`, like using `warn` for writing warnings or lower-level APIs like `$stderr.write`. Implementation-wise, `stderr` is unbuffered by default, ensuring immediate output, whereas `stdout` is typically buffered.

## See Also
- Ruby documentation on I/O: [https://ruby-doc.org/core-3.1.2/IO.html](https://ruby-doc.org/core-3.1.2/IO.html)
- The Open Group Base Specifications (UNIX standard streams): [https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html)
- Understanding Shell script redirection: [https://www.gnu.org/software/bash/manual/html_node/Redirections.html](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
