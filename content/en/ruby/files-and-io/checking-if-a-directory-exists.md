---
title:                "Checking if a directory exists"
aliases: - /en/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:02:26.224094-07:00
model:                 gpt-4-0125-preview
simple_title:         "Checking if a directory exists"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists in Ruby allows programmers to verify the presence of a directory before performing operations like reading files or creating new directories. This is crucial for avoiding errors in file handling and ensuring the reliability of file system manipulations.

## How to:
Ruby's standard library provides straightforward methods to check for a directory's existence. Here's how you do it with pure Ruby, without needing any third-party libraries:

```ruby
require 'fileutils'

# Check if a directory exists
if Dir.exist?('/path/to/directory')
  puts 'Directory exists.'
else
  puts 'Directory does not exist.'
end
```
Sample Output:
```
Directory exists.
```
Or:
```
Directory does not exist.
```

In addition to using `Dir.exist?`, you can also utilize the `File.directory?` method which returns `true` if the given path is a directory:

```ruby
if File.directory?('/path/to/directory')
  puts 'Directory exists.'
else
  puts 'Directory does not exist.'
end
```
Both `Dir.exist?` and `File.directory?` are part of Ruby's standard library and do not require any external gems to use, making them convenient and efficient options for directory checks.
