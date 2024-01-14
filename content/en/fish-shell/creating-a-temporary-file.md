---
title:                "Fish Shell recipe: Creating a temporary file"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Why

Creating temporary files in programming is a common task that allows for efficient and organized data management. Temporary files are used to store data temporarily during the execution of a program, and they are automatically deleted once the program has finished running. This helps avoid clutter and frees up space on the system.

##How To

To create a temporary file in Fish Shell, we will be using the `mktemp` command. This command generates a unique and secure filename that can be used for our temporary file.

```
Fish Shell

set TEMP_FILE (mktemp)
echo "Hello world!" > $TEMP_FILE
cat $TEMP_FILE 
```

Running the above code will create a temporary file named "tmp.XXXXXX" and write the text "Hello world!" into it. The `TEMP_FILE` variable is set to the filename of the temporary file, and we can use it to perform operations on the file, such as reading or writing data.

##Deep Dive

When creating a temporary file, it is important to consider security and preventing potential conflicts with other temporary files. The `mktemp` command in Fish Shell uses the `mkstemp()` function in C, which creates a file with a unique name and sets the appropriate permissions to ensure that only the user has access to the file.

It is also possible to create a temporary file with a specific prefix or suffix, using the `-p` and `-s` options respectively. This can be helpful when organizing and identifying temporary files in a busy system.

Additionally, we can use the `trap` command in Fish Shell to automatically delete the temporary file once the program finishes running. This can be achieved by setting up a trap command with the `EXIT` signal, which will execute a given command when the program exits.

##See Also

- Fish Shell built-in commands: https://fishshell.com/docs/current/index.html#builtin-commands
- `mktemp` command documentation: https://fishshell.com/docs/current/cmds/mktemp.html
- `mkstemp()` function: https://linux.die.net/man/3/mkstemp