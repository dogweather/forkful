---
title:                "Bash recipe: Creating a temporary file"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common task in Bash programming. It allows you to store temporary data or results of a process without cluttering up your main system files. This can be especially useful when dealing with large amounts of data or when running multiple processes simultaneously.

## How To

To create a temporary file in Bash, you can use the `mktemp` command. Simply type `mktemp` followed by a file name pattern, such as `tempfileXXX`. The `XXX` will be automatically replaced with a unique identifier, ensuring that each temporary file has a unique name.

```Bash
$ mktemp tempfileXXX
/tmp/tempfileCo4
```
In the example above, the temporary file created is called "tempfileCo4". You can also specify a different directory for the temporary file to be created in by using the `-p` option.

```Bash
$ mktemp -p ~/Documents tempfileXXX
/home/username/Documents/tempfileRer
```

Once you have created a temporary file, you can use it just like any other regular file in your script. For example, you can output data to the file using `echo` and then access the data later on.

```Bash
$ echo "Hello World!" > tempfileXXX
$ cat tempfileXXX
Hello World!
```

Once your script has finished running, you can use the `rm` command to delete the temporary file. This ensures that your system does not get cluttered with unnecessary files.

```Bash
$ rm tempfileXXX
```

## Deep Dive

Behind the scenes, the `mktemp` command uses the `mkstemp()` function from the C standard library. This function creates a unique temporary file with permissions only accessible by the user who created it. This helps to prevent accidental deletion or modification of the file by other users on the system.

Additionally, the `mktemp` command provides options for specifying the file suffix, prefix, and even the directory to create the temporary file in. This allows for more customization and flexibility when creating temporary files in Bash.

## See Also

- [Bash scripting cheat sheet](https://devhints.io/bash)
- [Introduction to Bash programming](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
- [Bash scripting tutorials](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)