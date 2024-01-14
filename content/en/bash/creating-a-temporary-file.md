---
title:    "Bash recipe: Creating a temporary file"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to store and manipulate data temporarily in your Bash scripts? If so, then you may have come across the concept of temporary files. These are essential for storing intermediate data that needs to be used and discarded quickly. In this blog post, we will explore how to create temporary files in Bash and why they are useful.

## How To

Creating a temporary file in Bash is a relatively simple process. We can use the `mktemp` command to generate a unique and secure temporary file. Let's take a look at an example:

```Bash
#!/bin/bash
temp_file=$(mktemp) # $() captures the output of the command
echo "This is a temporary file created at $(date)" > $temp_file # redirect output to the temporary file
cat $temp_file # print the contents of the temporary file
rm $temp_file # delete the temporary file
```

Running this script will create a temporary file with a random name and print its contents, which in this case will be the current date and time. After the script is finished, the temporary file is deleted. 

We can also specify a prefix for the temporary file's name using the `-p` flag. This can be helpful for identifying which script or process created the temporary file. For example:

```Bash
#!/bin/bash
temp_file=$(mktemp -p my_script) # temporary file name will be `my_script.XXXX`
echo "This is a temporary file created at $(date)" > $temp_file
cat $temp_file
rm $temp_file
```

Another useful option is the `--suffix` flag, which allows us to add a custom suffix to the temporary file's name. For example:

```Bash
#!/bin/bash
temp_file=$(mktemp --suffix .txt) # temporary file name will have `.txt` extension
echo "This is a temporary file created at $(date)" > $temp_file
cat $temp_file
rm $temp_file
```

## Deep Dive

The `mktemp` command not only creates a temporary file but also ensures its security by assigning proper permissions to it. This ensures that the file can only be accessed by the user running the script. Additionally, the temporary file's name is random, making it difficult for an attacker to guess and access.

Furthermore, the `mktemp` command also creates the temporary file in the appropriate temporary directory for the system, making it easier to manage and clean up later. The temporary directory is typically `/tmp`, but this can vary depending on the system.

It is worth noting that there is another command called `tempfile` that can also be used to create temporary files in Bash. However, unlike `mktemp`, `tempfile` creates the temporary file in the current directory and does not provide the same level of security.

## See Also

- [More information on `mktemp` command](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Creating and using temporary files in Bash](https://www.baeldung.com/linux/create-temporary-file-bash)
- [Handling temporary files in Bash](https://linuxhint.com/handle_temporary_files_bash/)