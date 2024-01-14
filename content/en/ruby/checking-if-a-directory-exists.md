---
title:    "Ruby recipe: Checking if a directory exists"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

When writing a Ruby program, it may sometimes be necessary to check if a certain directory or folder exists before performing any operations on it. This could be for error handling purposes or to ensure that the program runs smoothly without interruption.

## How To

To check if a directory exists in Ruby, we can use the `Dir.exist?` method. First, we need to require `Dir` in our code. Then we can use the `Dir.exist?` method and pass in the name of the directory as an argument.

```
require 'Dir'
puts Dir.exist?("Documents")
```

In the example above, we require the `Dir` module and check if the "Documents" directory exists. If the directory exists, the program will output `true`. Otherwise, it will output `false`.

## Deep Dive

The `Dir.exist?` method uses the `File::Stat` class to check for the existence of a directory. It returns a boolean value, `true` if the directory exists and `false` if it does not. This method also takes into consideration any symbolic links and will return `true` if the file path points to a valid directory.

It is also worth noting that the `Dir.exist?` method only checks for the existence of the directory itself, not its contents. So even if the directory is empty, this method will still return `true`.

## See Also

- Ruby documentation on `Dir.exist?`: https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F
- Tutorial on checking file and directory existence in Ruby: https://www.rubyguides.com/2015/03/ruby-file-exists/