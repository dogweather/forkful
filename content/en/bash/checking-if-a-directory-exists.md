---
title:                "Bash recipe: Checking if a directory exists"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

As a programmer, it is common to encounter situations where you need to check if a directory exists before performing certain operations. This is important because your code should be able to handle different scenarios, including when the directory you need does not exist.

## How To

To check if a directory exists in Bash, there are a few options to choose from depending on your requirements. One way is to use the ```-d``` flag with the ```test``` command. This command checks if a given path is a directory or not, and returns a boolean value.

```
if test -d /path/to/directory; then
  echo "The directory exists!"
else
  echo "The directory does not exist."
fi
```

Another way is to use the ```-e``` flag with the ```test``` command, which checks if the given path exists or not. This can be useful since directories are also considered as files in Linux.

```
if test -e /path/to/directory; then
  echo "The directory exists!"
else
  echo "The directory does not exist."
fi
```

You can also use the ```-d``` flag with the ```[``` built-in command, which is essentially the same as using the ```test``` command.

```
if [ -d /path/to/directory ]; then
  echo "The directory exists!"
else
  echo "The directory does not exist."
fi
```

Lastly, you can use the ```-d``` flag with the ```[[``` keyword, which is a more modern and powerful version of the ```[``` command and supports some additional features.

```
if [[ -d /path/to/directory ]]; then
  echo "The directory exists!"
else
  echo "The directory does not exist."
fi
```

## Deep Dive

Behind the scenes, the ```-d``` and ```-e``` flags work by checking the given path against the filesystem's inode table. If the path exists and is a directory, it will return a successful exit code (0), otherwise, it will return an error code (1).

It is worth noting that symbolic links to directories will also return a successful exit code, so if you specifically want to check for a *regular* directory, you may need to use the ```-d``` flag with the ```readlink``` command to follow any symbolic links and then compare the output.

```
if [[ $(readlink -f /path/to/link) == /path/to/directory ]]; then
  echo "The link points to a regular directory!"
fi
```

## See Also

- [Bash Manual - Conditional Expressions](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Conditional-Expressions)
- [Linux Shell Scripting Tutorial - File Test Operators](https://www.shellscript.sh/test.html)
- [BashTutorial.net - Testing in Bash](https://www.bashtutorial.net/special-variables/shell-test-operators.html)