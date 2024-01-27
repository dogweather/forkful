---
title:                "Modifying files with CLI one-liners"
date:                  2024-01-26T22:08:17.373561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modifying files with CLI one-liners"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## What & Why?
Modifying files with CLI (Command Line Interface) one-liners in Ruby involves performing quick and often simple text manipulations directly from the terminal using Ruby's command line options. This technique is invaluable when you need to make batch changes to files, filter content, or automate editing tasks without opening an editor. It's about leveraging Ruby's text processing capabilities efficiently for scriptable edits.

## How to:
Consider you have a file named `example.txt` with several lines of text and you wish to reverse the lines' order. With Ruby, you can accomplish this in a one-liner:

```ruby
ruby -e 'puts File.readlines("example.txt").reverse' 
```

Or, if you want to replace all occurrences of "foo" with "bar" in `data.txt`, you can do:

```ruby
ruby -i.bak -pe 'gsub(/foo/, "bar")' data.txt
```

This command also creates a backup (`data.txt.bak`) of the original file, showcasing Ruby's consideration for data safety. Sample output is not directly visible as these commands change file content, but you can `cat data.txt` to view the changes.

## Deep Dive
The `-e` flag tells Ruby to execute the given script, while `-i` enables in-place editing with an optional extension to create a backup file. The `-p` flag loops through the input and prints each line after the script is applied, akin to sed in Unix/Linux.

Historically, in-place editing and command line processing were territories dominated by sed, awk, and perl. Ruby, however, incorporates these functionalities nicely, allowing for more complex manipulations due to its rich syntax and built-in libraries.

Alternatives for file modification include sed and awk for simpler tasks, or using full Ruby scripts for more complex processing. The downside of using Ruby for one-liners might be performance for very large files or complex operations, where tools designed specifically for text processing might run faster.

Implementation-wise, when Ruby processes files in-line, it effectively creates a temporary output while reading the file, then replaces the original file with this output. This detail underscores the importance of backup options or careful testing with `-i` flag usage to avoid data loss.

## See Also
- Ruby's official documentation on command-line options: [https://www.ruby-lang.org/en/documentation/quickstart/3/](https://www.ruby-lang.org/en/documentation/quickstart/3/)
- An extensive comparison of text processing in Ruby vs. sed and awk: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- For a deeper dive into Ruby's handling of files and IO: [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)