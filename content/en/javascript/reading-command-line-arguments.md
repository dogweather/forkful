---
title:    "Javascript recipe: Reading command line arguments"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

As a programmer, you may come across a situation where you need to get input from the user in order to run your code. This is where reading command line arguments becomes useful. By reading these arguments, you can customize your program based on the user's input, making it more dynamic and interactive.

## How To

In Javascript, reading command line arguments is a simple process. First, you need to declare a variable to hold the arguments. This can be done using the `process.argv` method. Next, you can access each argument using its index, starting with 2. The first argument at index 0 is the path to the Node.js executable and the second argument at index 1 is the path to the current file.

Let's see an example of reading command line arguments in action:

```Javascript
//index.js

//declare a variable to hold the arguments
let args = process.argv;

//access the first argument (index 2)
console.log("Hello, " + args[2] + "! Welcome to my program.");
```

If we run this code with the following command line arguments:

```
node index.js John
```

The output would be:

```
Hello, John! Welcome to my program.
```

You can also use a loop to access all the arguments, in case you need to process multiple inputs:

```Javascript
//index.js

//declare a variable to hold the arguments
let args = process.argv;

//loop through the arguments starting at index 2
for (let i = 2; i < args.length; i++) {
    console.log("Argument " + (i - 1) + ": " + args[i]);
}
```

If we run this code with the same command line arguments as before, the output would be:

```
Argument 1: John
```

## Deep Dive

Behind the scenes, the `process.argv` method returns an array of strings, with the first two elements being the paths mentioned earlier. The rest of the elements in the array are the arguments passed by the user. You can also use the `slice()` method to get rid of the first two elements and access only the arguments themselves.

It's worth noting that command line arguments cannot handle spaces. So if you need to pass in a string with spaces, you can wrap it with quotation marks. For example:

```
node index.js "Hello World"
```

## See Also

- [Node.js Documentation - Process Global](https://nodejs.org/api/process.html#process_process_argv)
- [How to get command line arguments in Node.js](https://www.digitalocean.com/community/tutorials/how-to-get-command-line-arguments-in-node-js)