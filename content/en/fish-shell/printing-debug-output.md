---
title:                "Fish Shell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

##Why

At first glance, printing debug output may seem like a tedious task with little benefit. However, as developers, it is crucial to have a deep understanding of our code and to be able to troubleshoot any potential errors. This is where printing debug output becomes a valuable tool. By printing out the value of certain variables or the execution of specific functions, we can gain valuable insights and catch any bugs early on in the development process.

##How To

To print debug output in Fish Shell, we can use the `echo` command. This command takes in a string or a variable and prints it to the terminal. Let's take a look at a simple example:

```Fish Shell
set my_variable "Hello from Fish Shell!"
echo $my_variable
```

In the above code, we first create a variable called `my_variable` and set its value to "Hello from Fish Shell!". Then, we use the `echo` command to print out the value of our variable. This will result in the following output in the terminal:

```Output
Hello from Fish Shell!
```

We can also use `echo` within loops or functions to print out the values of different variables at different stages of our code. This allows us to track the execution of our code and spot any potential issues.

##Deep Dive

Printing debug output can also be helpful when working with more complex data structures, such as arrays or dictionaries. We can use the `printf` command to format our output in a more readable way. For example:

```Fish Shell
set my_array (seq 1 5)
printf "The array is: %s\n" $my_array
```

In this code, we first create an array with values from 1 to 5 using the `seq` command. Then, we use the `printf` command to print out the array's values in a formatted manner. The `%s` represents where the array values will be inserted in the string, and the `\n` adds a line break after each element. This will result in the following output:

```Output
The array is: 1 2 3 4 5
```

By using `echo` and `printf` with different formatting options, we can easily print out the values of more complex data structures for debugging purposes.

##See Also

- Official Fish Shell Documentation for `echo` and `printf`: https://fishshell.com/docs/current/commands.html#echo and https://fishshell.com/docs/current/commands.html#printf
- A Beginner's Guide to Debugging in Fish Shell: https://codementor.io/@jamesezechukwu/a-beginner-s-guide-to-debugging-in-fish-shell-svjs6p8lv
- Debugging in Fish Shell: https://www.rabartu.net/posts/debugging-in-fish-shell/