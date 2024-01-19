---
title:                "Checking if a directory exists"
html_title:           "Bash recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Bash entails programming it to verify the existence of a certain folder. It's vital to avoid errors when addressing files or folders that may not exist, ensuring your code runs smoothly.

## How to:

```Bash
# Check if directory exists
if [ -d "$DIRECTORY" ]; then
  echo "Directory exists"
else
  echo "Directory does not exist"
fi
```
This code checks whether `$DIRECTORY` exists. If it does, it prints "Directory exists." If not, it prints "Directory does not exist."

## Deep Dive

Historically, file checking has been integral to operating systems, and Bash, born from the Unix tradition, is no exception. It employs the old but gold `-d` option as seen above. 

As for alternatives, `test` can be used in place of the `[... ]` syntax. Check out this syntax variety:

```Bash
if test -d "$DIRECTORY"; then
  echo "Directory exists"
else
  echo "Directory does not exist"
fi
```
It functions similarly to the first example.

Implementation details include how `-d` actually works. It checks if something exists and is a directory. Other options like `-f` can be used to check if something exists and is a file.

## See Also

For further reading, there are many Bash tutorials and resources online. However, the [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html) is superb, especially its section on the `-d` file test operator.