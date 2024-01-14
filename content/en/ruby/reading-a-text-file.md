---
title:    "Ruby recipe: Reading a text file"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a fundamental skill for any programmer. It allows us to access and manipulate large amounts of data, making our programs more dynamic and powerful. In this blog post, we will explore how to read a text file using Ruby, a popular and versatile programming language.

## How To

To start, we will need a text file to read. Create a file called "blog_posts.txt" and add the following content:

```
Title: Ruby Programming
Author: John Smith
Date: 2021-04-20
```

Next, in our Ruby program, we will use the `File.open` method to open the text file and read its contents. Here is an example:

```Ruby
file = File.open("blog_posts.txt", "r")

content = file.read

puts content
```

The `File.open` method takes two arguments - the name of the file we want to open and the mode in which we want to open it. In this case, we use "r" to indicate that we want to read the file.

The `read` method then reads the entire content of the file and stores it in the `content` variable. Finally, we use the `puts` method to print the contents of the file.

When we run this program, the output will be:

```
Title: Ruby Programming
Author: John Smith
Date: 2021-04-20
```

We can also read the file line by line using the `each_line` method. Here is an example:

```Ruby
file = File.open("blog_posts.txt", "r")

file.each_line do |line|
  puts line
end
```

This will print the contents of the file line by line:

```
Title: Ruby Programming
Author: John Smith
Date: 2021-04-20
```

We can also use the `File.readlines` method to store each line of the file in an array. Here is an example:

```Ruby
file = File.open("blog_posts.txt", "r")

content_array = file.readlines

puts content_array[0]
```

This will print the first line of the file:

```
Title: Ruby Programming
```

## Deep Dive

When we use the `File.open` method, we also have the option to specify an encoding for the file. This is important when working with text files that use a specific character set, such as UTF-8 or ASCII.

For example, if we have a text file with Japanese characters, we can specify the encoding as UTF-8 when opening the file:

```Ruby
file = File.open("japanese_blog_posts.txt", "r:UTF-8")
```

We can also use the `File.read` method to read a specific number of characters from the file. Here is an example:

```Ruby
file = File.open("blog_posts.txt", "r")

content = file.read(10)

puts content
```

This will print the first 10 characters of the file:

```
Title: Ruby
```

## See Also

- [File Class - Ruby documentation](https://ruby-doc.org/core-3.0.1/File.html)
- [Ruby File I/O - GeeksforGeeks](https://www.geeksforgeeks.org/ruby-file-io/)
- [Reading and Writing Files in Ruby - Codecademy](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-control-flow-u/articles/ruby-reading-and-writing-files)