---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Bash 101: Reading a Text File Like a Pro

## What & Why?

Simply put, reading a text file means accessing its contents line-by-line or as a whole. Programmers do it to handle data, be it config stuff, logs, or other program output.

## How to:

To get a file's content line by line, use a `while` loop and `read`. Each line flows into a variable:

```Bash
while IFS= read -r line
do
    echo "$line"
done < "myfile.txt"
```

For the full thing, just `cat` it!

```Bash
cat myfile.txt
```

Output _exactly_ matches your file's content.

## Deep Dive:

Originally, Bash (Bourne-again shell) from '89 didn't have file I/O! It used external progs like `cat`. Now, it's built-in, and it's faster.

Alternatives? Sure, you've got Perl, Python etc. for more complex jobs, but for speed and simplicity, nothing beats Bash. 

About implementation. When Bash reads, it buffers stuff. Means less disk access, which means speed. Note: no EOF (end-of-file) test necessary in the loop, 'cause `read` returns false once there's nothing more.

## See Also:

- Man page for [Bash's built-in commands](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)
- A [quick tutorial](https://www.learnshell.org/) to level up your shell scripting.
- For a deep dive, check out the [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) on The Linux Documentation Project's website!
Happy scripting!