---
title:                "Reading command line arguments"
html_title:           "Bash recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 
If you're a programmer, chances are you've heard about Bash. This popular command line interpreter allows you to automate tasks and improve productivity. One helpful feature of Bash is the ability to read command line arguments, making it easier to pass information to your scripts. 

## How To 
To start, open a Terminal and navigate to the directory where your Bash script is located. Then, follow these steps to read command line arguments in Bash: 

1. Create a new Bash file by using the command `touch script.sh`. This will create a blank file named "script" with the extension ".sh".
2. Add the necessary Bash shebang at the top of your script file by typing `#!/bin/bash`. This tells the computer to run the script using the Bash interpreter.
3. Define a function to read the command line arguments by using the `read` command followed by the variable name. For example, `read input` will store the first argument in the variable "input".
4. To access the arguments, use the `echo` command followed by the variable name. For example, `echo $input` will print the first argument provided when running the script.
5. Save and exit the file, then run your Bash script by typing `./script.sh argument1 argument2`. The arguments will be read and displayed in the terminal.

```
```Bash
#!/bin/bash

# defining function to read command line arguments
read input1
read input2

# accessing arguments using echo command
echo $input1
echo $input2
```
```

Sample output:
```
$ ./script.sh hello world 
hello
world 
```

## Deep Dive 
When reading command line arguments in Bash, there are a few things to keep in mind:

1. The first argument is stored in the variable `$1`, the second argument in `$2` and so on.
2. You can use the `$#` variable to find the total number of arguments passed to the script.
3. Arguments are separated by spaces, so if an argument contains a space, it should be enclosed in quotes when running the script. For example, `./script.sh "hello world"` will treat "hello world" as one argument.
4. You can also use the `shift` command to remove the first argument and make the second argument the new first argument. This can be useful if you want to handle each argument separately in a loop.

## See Also
- [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Command line arguments in Bash](https://www.baeldung.com/linux/bash-command-line-arguments)
- [Bash scripting cheat sheet](https://devhints.io/bash)