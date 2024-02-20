---
date: 2024-01-20 17:53:45.419439-07:00
description: "Reading a text file means getting the content from a file into your\
  \ script. Programmers do it to work with data, configuration, or to automate systems\u2026"
lastmod: 2024-02-19 22:05:18.722767
model: gpt-4-1106-preview
summary: "Reading a text file means getting the content from a file into your script.\
  \ Programmers do it to work with data, configuration, or to automate systems\u2026"
title: Reading a text file
---

{{< edit_this_page >}}

## What & Why?
Reading a text file means getting the content from a file into your script. Programmers do it to work with data, configuration, or to automate systems based on that text-file content.

## How to:
Here's the simplest way to read a file line-by-line:

```Bash
while IFS= read -r line; do
    echo "Text: $line"
done < "yourfile.txt"
```

Want the whole file at once? Try this:

```Bash
file_content=$(<yourfile.txt)
echo "$file_content"
```

Or do you need a specific line, say line 4?

```Bash
sed '4q;d' yourfile.txt
```

Sample output for reading line 4:

```
This is the content of line four.
```

## Deep Dive
Back in the day, we didn't have fancy IDEs, we had terminals and simple text editors. UNIX tools were designed with a philosophy of doing one thing well. `cat`, `less`, `sed`, and `awk` are veterans at manipulating text. 

Reading a file in Bash leverages these tools, plus Bash's own redirects and loops. For example, using `while` with `read` is good for memory efficiency with large files. You're reading line by line, not dumping everything in memory.

`sed` is a stream editor. Grabbing a specific line with `sed '4q;d' yourfile.txt` tells `sed` to quit after line 4 (`4q`) and then print (`;d`) that line.

Alternatives exist. `awk` is powerful for text processing. Perl and Python scripts can be invoked within Bash when text processing gets complex. Each of these tools and languages has its own use cases and performance considerations.

## See Also
1. Bash Scripting Guide: https://www.gnu.org/software/bash/manual/
2. `sed` and `awk` 101 Hacks: https://www.thegeekstuff.com/2009/12/unix-sed-tutorial-6-examples-to-edit-file-in-place/
3. Linux Command Line Text Processing with `grep`, `awk`, `sed`, `sort`, and friends: https://github.com/learnbyexample/Command-line-text-processing
