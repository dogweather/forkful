---
title:                "Creating a temporary file"
html_title:           "Fish Shell recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files can be useful when working with large amounts of data or when running complex scripts that require temporary storage. It allows the user to store and manipulate data without permanently altering existing files.

## How To

Creating a temporary file in Fish Shell is a simple process. First, we must use the built-in `mktemp` command to generate a unique name for our temporary file. We can do this by running the following command:

```
Fish Shell code block: 
```console
$ tempfile=(mktemp)
```

This will create a variable named `tempfile` with a unique name for our temporary file. Next, we can use the `echo` command to write data to our temporary file. For example, we can write the text "Hello World" to our file by running the following command:

```
Fish Shell code block:
```console
$ echo "Hello World" > $tempfile
```

We can then use the `cat` command to view the contents of our temporary file:

```
Fish Shell code block:
```console
$ cat $tempfile
Hello World
```

Once we are finished using the temporary file, we can use the `rm` command to remove it from our system:

```
Fish Shell code block:
```console
$ rm $tempfile
```

## Deep Dive

When we use the `mktemp` command, it generates a unique name for our temporary file based on a set of rules. By default, it will create a temporary file in the `/tmp` directory with a name in the format of `tmp.XXXXXX`. The `X` characters are replaced with random letters and numbers to ensure a unique name.

However, the `mktemp` command also allows for customization of the temporary file name. By adding a prefix or suffix to the command, we can have more control over the name of our temporary file. For example, we can specify a prefix of `test` for our temporary file by running `mktemp test.XXXXXX`.

Additionally, the `mktemp` command has options for creating temporary directories and setting permissions on the file created.

## See Also

Check out these links for more information on creating temporary files in Fish Shell:

- Official Fish Shell documentation on `mktemp` command: https://fishshell.com/docs/current/cmds/mktemp.html
- Tutorial on using temporary files in Fish Shell: https://techstop.github.io/fish-shell-tutorial/2017/09/01/using-tmp-with-fish.html
- Discussion forum on the benefits of using temporary files in scripting: https://superuser.com/questions/1092753/the-benefit-of-using-temp-files-in-scripts