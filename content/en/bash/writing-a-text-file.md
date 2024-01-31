---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file is the process of saving data into a file in text format. Programmers do it to store configurations, logs, code, or any data that need to be referenced or preserved over time.

## How to:

```Bash
# Creating a new text file with the 'echo' command
echo "Hello, World!" > hello.txt

# Appending more text to an existing file with the '>>' operator
echo "Another line of text." >> hello.txt

# Writing multiple lines using a heredoc
cat << EOF > hello_multiline.txt
Hello, this is the first line.
And this is the second line.
EOF
```

Output for `cat hello.txt`:
```
Hello, World!
Another line of text.
```

Output for `cat hello_multiline.txt`:
```
Hello, this is the first line.
And this is the second line.
```

## Deep Dive

Shell scripting has been a core part of Unix-like systems since the 1970s, with the `sh` (Bourne shell) being the original. Today, `bash` (Bourne Again SHell) is a widely available and used shell. While `echo` and output redirection (`>`, `>>`) are common methods to write files, alternatives like `printf` offer formatting capabilities. File writing in bash scripts use file descriptors; `1` for `stdout`, and appending (`>>`) avoids file overwriting by leveraging file descriptor `2`.

## See Also

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- [Shell Scripting Tutorial](https://www.shellscript.sh/)
