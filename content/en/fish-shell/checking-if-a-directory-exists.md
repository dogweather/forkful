---
title:                "Fish Shell recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why 
As a fish shell programmer, you may find yourself needing to check if a directory exists before executing a certain command or script. This can save you from running into errors and make your code more efficient.

## How To
To check if a directory exists in Fish Shell, you can use the `test` command with the `-d` option. This command checks if a given path exists and is a directory. Here's an example code block:

```
Fish Shell Code Block
if test -d /path/to/directory
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

If the `/path/to/directory` exists, the output will be "Directory exists". If it doesn't exist, the output will be "Directory does not exist". 

## Deep Dive
For a deeper understanding, let's break down the code block above. The `if` statement checks the output of the `test` command. If it returns true, the first block of code is executed. If it returns false, the `else` statement is executed. 

The `-d` option specifically checks if the given path exists and is a directory. In this case, the path is the `/path/to/directory` that we are checking. 

## See Also
- [Fish Shell documentation on test command](https://fishshell.com/docs/current/cmds/test.html)
- [Fish Shell tutorial on file tests](https://fishshell.com/docs/current/tutorial.html#file-tests)
- [Linux man page for test command](https://man7.org/linux/man-pages/man1/test.1.html)