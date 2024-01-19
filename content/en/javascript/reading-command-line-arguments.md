---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments means accessing values directly passed to your Javascript script via the command line. This gives your code the ability to interact with inputs in a dynamic way, further enhancing its functionality.

## How to:

In NodeJS, command line arguments can be accessed via the `process.argv` array. Here's a basic example of how to get your command line arguments:

```Javascript
// Command
// $ node myScript.js arg1 arg2

process.argv.forEach((val, index) => {
    console.log(`${index}: ${val}`);
});

// Output
// 0: /…/.nvm/versions/node/v14.15.0/bin/node
// 1: /Users/username/myScript.js
// 2: arg1
// 3: arg2
```

The first two elements of `process.argv` are path to `node` and `your-script.js` file respectively. The following elements are your command line arguments. Note that they're all strings!

## Deep Dive

Historically, command line arguments have been a useful way of passing parameters to programs back when Graphical User Interfaces (GUIs) were not popular. They still hold their grounds for their simplicity and effectiveness.

An alternate to `process.argv` is the `minimist` library. It parses command line arguments and options with more ease and provides them in a convenient format, such as providing an object instead of an array.

In NodeJS underlying architecture, the `process.argv` array is supplied by the C `main(argc, argv, env)` function. It becomes global as NodeJS wraps it into the global `process` object.

## See Also

- More about the process.argv: [NodeJS Process Docs](https://nodejs.org/api/process.html#process_process_argv)
- Minimist NPM Package: [Minimist NPM](https://www.npmjs.com/package/minimist)