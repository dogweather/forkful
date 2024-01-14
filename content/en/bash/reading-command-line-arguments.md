---
title:    "Bash recipe: Reading command line arguments"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Have you ever wondered what those little arguments after a command in your Terminal or command line are for? Maybe you've seen others using them or have come across them while reading online tutorials. Well, wonder no more! In this blog post, we will delve into the world of command line arguments and why they are an important aspect of Bash programming.

## How To

Let's start with the basics. Command line arguments are additional pieces of information that can be passed to a Bash script or program when it is run. They allow us to customize and modify the behavior of our scripts without having to edit the code itself. So, how do we use them?

To start, let's create a simple Bash script that will say hello to the user based on their name. In your preferred text editor, create a new file called "hello.sh" and add the following code:

```Bash
#!/bin/bash

echo "Hello, $1!"
```

In this script, we have used the special variable "$1" to represent the first command line argument passed to the script. This means that when we run the script, whatever is typed after the script name will be printed after the word "Hello." For example, if we run the following command:

```
./hello.sh John
```

The output will be:

```
Hello, John!
```

We can also use multiple command line arguments by using the variables $2, $3, and so on for the second, third, and subsequent arguments.

```
#!/bin/bash

echo "Hello, $1 and $2!"
```

Running the command:

```
./hello.sh John Sarah
```

Will result in the output:

```
Hello, John and Sarah!
```

Of course, we can do much more with command line arguments, such as passing in options or flags to our scripts. For example, we can use the getopts command to read in options and arguments and perform different actions based on them. The possibilities are endless!

## Deep Dive

For those who want to take their knowledge of command line arguments to the next level, there are a few things to note. Firstly, arguments are numbered in the order they are passed to the script, starting with $0 for the name of the script itself. This means that the first argument after the script name will be $1, the second will be $2, and so on.

Additionally, arguments that contain spaces or special characters should be surrounded by quotes to avoid causing errors. For example, if we wanted to pass in the name "John Smith," we would use quotes like this:

```
./hello.sh "John Smith"
```

Lastly, it's important to note that the order in which arguments are passed to a script matters. If you were to switch the positions of the arguments in the previous example, the output would be:

```
Hello, Sarah and John!
```

See Also

- [The Bash Guide on Command Line Arguments](https://www.gnu.org/software/bash/manual/html_node/Command-Line-Arguments.html)
- [How to Use Command Line Arguments in Bash Scripts](https://linuxize.com/post/bash-script-get-arguments/)
- [Advanced Bash-Scripting Guide: Chapter 34. Special Characters](https://tldp.org/LDP/abs/html/special-chars.html#ARGPOSITION)