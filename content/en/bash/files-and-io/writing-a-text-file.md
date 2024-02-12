---
title:                "Writing a text file"
aliases:
- en/bash/writing-a-text-file.md
date:                  2024-02-03T19:03:24.379648-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing a text file"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Bash allows you to automate data storage, logging, configuration settings, and more. It's a fundamental skill for shell scripting, enabling programmers to save the output of commands, script executions, or user input for reporting, processing, or future execution.

## How to:

Bash provides straightforward methods for writing to a file. The most common are using redirection operators (`>`, `>>`) and the `tee` command. Here's a quick look at both techniques.

Using redirection, you can write output directly to a file. The `>` operator writes content to a file, replacing it if it already exists, while `>>` appends to an existing file without deleting its content.

```bash
# Writing to a file with >
echo "Hello, World!" > myfile.txt

# Appending to a file with >>
echo "This is a new line." >> myfile.txt
```

If you check the content of `myfile.txt` after running the above commands, you'd find:

```
Hello, World!
This is a new line.
```

The `tee` command is handy when you want to write to a file and see the output on the screen (stdout) simultaneously. By default, `tee` overwrites the file, but with the `-a` flag, it appends to the file.

```bash
# Writing and displaying using tee
echo "Hello, again!" | tee myfile.txt

# Appending and displaying using tee -a
echo "Adding another line." | tee -a myfile.txt
```

After running these, `myfile.txt` will display:

```
Hello, again!
Adding another line.
```

While Bash itself provides robust file manipulation capabilities through redirection and commands like `tee`, further manipulation or more complex scenarios might require calling external tools or scripting languages (e.g., Awk, Sed, Python) that offer more sophisticated text processing functions. However, for most straightforward file writing tasks, the above methods are fully sufficient and widely used.
