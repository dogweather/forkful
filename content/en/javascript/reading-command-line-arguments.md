---
title:    "Javascript recipe: Reading command line arguments"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why

When it comes to programming, there are often different ways to achieve the same result. One such example is reading command line arguments in Javascript. While not commonly used, understanding how to read and manipulate command line arguments can be a valuable skill for a programmer. In this blog post, we will explore why someone would engage in reading command line arguments and how to effectively do so in Javascript.

##How To

To read command line arguments in Javascript, we first need to access the built-in `process` object. This object provides information about the current Node.js process, including command line arguments.

We can access the command line arguments using the `process.argv` property, which returns an array of strings. The first element in this array will always be the path to the Node.js executable, followed by the path to the Javascript file being executed, and then any additional arguments provided in the command line.

To demonstrate this, let's create a simple Javascript file and run it with some arguments. In this example, we will create a file called `greeting.js` and add the following code:

```Javascript
// greeting.js
console.log(`Welcome to ${process.argv[2]} ${process.argv[3]}!`);
```

Now, if we run this file with the following command:

`node greeting.js John Doe`

The output will be:

`Welcome to John Doe!`

We can also access individual arguments by using their index within the `process.argv` array. For example, `process.argv[0]` would return the path to the Node.js executable, and `process.argv[1]` would return the path to the `greeting.js` file.

In addition to accessing the command line arguments, we can also manipulate them to fit our needs. For example, we can use the `slice()` method to create a new array with only the arguments we want. This can be useful if we only need specific arguments or want to ignore the first few elements in the `process.argv` array.

##Deep Dive

Under the hood, Node.js uses the `yargs` library for parsing command line arguments. This library allows for a more flexible and intuitive way of working with arguments, providing features such as parsing options and defining commands.

To use `yargs`, we first need to install it using `npm` by running the following command:

`npm install yargs`

Now, we can use `yargs` in our `greeting.js` file to achieve the same result as before, but in a more organized and dynamic way. Here's an example:

```Javascript
// greeting.js

const args = require('yargs').argv;

console.log(`Welcome to ${args.first} ${args.last}!`);
```

Now, if we run the file with the following command:

`node greeting.js --first="John" --last="Doe"`

The output will be:

`Welcome to John Doe!`

Note that `yargs` automatically parses the command line arguments and creates an `argv` object with key-value pairs based on the provided options. This makes it easier to access and manipulate the arguments in a more structured way.

##See Also

- [Node.js Process Object documentation](https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Yargs library documentation](https://www.npmjs.com/package/yargs)
- [Command Line Arguments in Node.js blog post](https://blog.logrocket.com/the-beginner-guide-to-command-line-arguments-in-node-js/)