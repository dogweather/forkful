---
title:                "Creating a temporary file"
html_title:           "Ruby recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming, especially in languages like Ruby where memory management is handled by the operating system. These temporary files serve as a temporary storage space for data and can be useful in various situations such as data processing, file handling, and testing.

## How To

To create a temporary file in Ruby, you can use the `Tempfile` class from the `tempfile` library. Here's an example:

```Ruby
require 'tempfile'

temp_file = Tempfile.new('my_temp_file') # specify a prefix for the file name
temp_file.write('This is a temporary file.') # write some data to the file
puts temp_file.path # print the path of the file
temp_file.close # close the file
```

Output:

```
/var/folders/5k/0426t1xj071d4plg5gwlz9m00000gn/T/my_temp_file20190917-35979-c2ri1s # the file path will be different for each execution
```

As you can see, the `Tempfile.new` method creates a file with a unique name in the system's temporary directory. You can also pass in an optional `mode` parameter to specify the file's permissions.

## Deep Dive

Behind the scenes, the `Tempfile` class uses the `File.open` method to create the temporary file. This means that all the methods available in the `File` class, such as `#write` and `#close` can be used on a `Tempfile` object as well.

One important thing to note is that the file will be automatically deleted when the `Tempfile` object is garbage collected or when the program terminates. If you want to explicitly delete the file, you can use the `#unlink` method.

## See Also

- [Official Ruby Documentation for Tempfile Class](https://ruby-doc.org/stdlib-2.6.5/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby Guides: Working with Temporary Files](https://www.rubyguides.com/2015/04/working-with-temporary-files/)