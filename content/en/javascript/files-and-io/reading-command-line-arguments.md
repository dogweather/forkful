---
date: 2024-01-20 17:56:05.403364-07:00
description: 'How to: Here''s the straight-up way to do it in Node.js.'
lastmod: '2024-03-13T22:45:00.448115-06:00'
model: gpt-4-1106-preview
summary: Here's the straight-up way to do it in Node.js.
title: Reading command line arguments
weight: 23
---

## How to:
Here's the straight-up way to do it in Node.js:

```javascript
// process.argv contains command line arguments
const args = process.argv.slice(2);

console.log(args);

// Run this script with: node yourscript.js firstArg secondArg
```

Sample output if you run `node yourscript.js pineapple 42`:

```javascript
['pineapple', '42']
```

Using a package like `yargs` makes life easier, letting you define and access arguments by name.

```javascript
// Install with npm install yargs
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// Run this with: node yourscript.js --fruit pineapple --number 42
```

And you'd get:

```javascript
{ fruit: 'pineapple', number: '42' }
```

Clean and clear, with named parameters.

## Deep Dive
Way back, arguments were read in C using `argc` and `argv` in the `main` function. In Node.js, `process.argv` is the go-to. It's an array where the first element is the path to the node executable, the second is the script file name, and the rest are your actual arguments.

`yargs` is nifty for complex apps: it parses arguments into a handy object, manages defaults, and even auto-generates help messages. 

There's also the `minimist` package, a lighter alternative to `yargs`, if you're into minimalism.

Deep down, Node.js uses V8's `process.binding('options')` for parsing which isn't exposed to the average user. This internal method packs tons of utility under the hood, managing the parsing and retrieval of command line options.

## See Also
- Node.js process.argv documentation: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Yargs GitHub repo: https://github.com/yargs/yargs
- Minimist on npm: https://www.npmjs.com/package/minimist
