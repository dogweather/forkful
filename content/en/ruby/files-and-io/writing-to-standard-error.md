---
date: 2024-02-03 19:03:35.880901-07:00
description: "How to: Ruby's standard library provides a straightforward way to write\
  \ to stderr using `$stderr` or `STDERR`. You don't need third-party libraries for\u2026"
lastmod: '2024-03-13T22:45:00.566292-06:00'
model: gpt-4-0125-preview
summary: Ruby's standard library provides a straightforward way to write to stderr
  using `$stderr` or `STDERR`.
title: Writing to standard error
weight: 25
---

## How to:
Ruby's standard library provides a straightforward way to write to stderr using `$stderr` or `STDERR`. You don't need third-party libraries for this basic operation.

### Writing a simple message to stderr:
```ruby
$stderr.puts "Error: File not found."
# Or equivalently
STDERR.puts "Error: File not found."
```
Sample output (to stderr):
```
Error: File not found.
```

### Redirecting stderr to a file:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Failed to open configuration."
end
```
This code snippet redirects stderr to a file named `error.log`, and all subsequent written errors will be outputted there until the program resets the stderr redirection or terminates.

### Using stderr with exception handling:
```ruby
begin
  # Simulating an operation that could fail, eg., opening a file
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Exception occurred: #{e.message}"
end
```
Sample output (to stderr):
```
Exception occurred: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

While Ruby's built-in methods for writing to stderr suffice for many applications, for more complex logging needs, you might consider the `logger` standard library or external gems like `Log4r`. These provide configurable logging mechanisms, including severity levels, formatting, and the ability to write to various outputs, including files, email, and more.
