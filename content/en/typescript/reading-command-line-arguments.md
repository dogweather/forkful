---
title:    "TypeScript recipe: Reading command line arguments"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why 
Reading command line arguments is an essential part of programming and can greatly improve the functionality and flexibility of your code. It allows you to pass data and input directly into your program without having to modify the source code. This can be especially useful when your program needs to be run multiple times with different input parameters.

## How To
To read command line arguments in a TypeScript program, we first need to access the `process` object which is available globally. Inside this object, we can access an array called `argv` which contains all the command line arguments passed to the program. Let's take a look at an example:

```TypeScript
const args = process.argv;
console.log(args);
```

Now, when we run this program and pass in some arguments, for example `node index.ts arg1 arg2`, we will get the following output:

```
[ 'node', '/path/to/index.ts', 'arg1', 'arg2' ]
```

As you can see, the first two arguments are always the absolute path to the Node.js binary and the path to the file being executed. The following arguments are the ones we have passed in ourselves. 

We can also access specific arguments by using the index of the `argv` array. For example, `args[2]` would give us the first argument we passed in.

```TypeScript
console.log(args[2]); // outputs 'arg1'
```

We can also use the `slice()` method to access a range of arguments.

```TypeScript
const args = process.argv.slice(2); // this will exclude the first two arguments
console.log(args); // outputs [ 'arg1', 'arg2' ]
```

## Deep Dive
Command line arguments are always passed in as strings, so if your program expects a different data type, you will need to convert it. This can be done using methods like `parseInt()` or `parseFloat()`.

Another important thing to note is that command line arguments are always passed in as a series of strings, even if they represent numbers or booleans. This means that you will need to handle any type conversions or validations in your code.

Also, it's important to properly handle any errors that may occur when reading command line arguments. For example, if the user doesn't pass in the expected number of arguments, your code should handle this and provide a meaningful error message.

Lastly, it's worth mentioning that there are libraries available, such as `yargs`, that can make working with command line arguments in TypeScript even easier. These libraries provide helpful functions and methods for parsing and validating arguments.

## See Also
- [Node.js process documentation](https://nodejs.org/api/process.html)
- [TypeScript documentation](https://www.typescriptlang.org/docs/home.html)
- [yargs library](https://www.npmjs.com/package/yargs)