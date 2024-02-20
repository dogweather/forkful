---
date: 2024-01-20 17:57:02.958419-07:00
description: Command line arguments let users pass data to a program when they run
  it. Programmers use them to customize a program's behavior without changing the
  code.
lastmod: 2024-02-19 22:05:18.342019
model: gpt-4-1106-preview
summary: Command line arguments let users pass data to a program when they run it.
  Programmers use them to customize a program's behavior without changing the code.
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?
Command line arguments let users pass data to a program when they run it. Programmers use them to customize a program's behavior without changing the code.

## How to:

In TypeScript, you use Node.js to read command line arguments. Here's how:

```typescript
// Needed to import process from Node.js
import process from 'process';

// Grab command line args from the third position onward
const args = process.argv.slice(2);

console.log('Command line arguments:', args);
```

Run this script like `ts-node yourscript.ts arg1 arg2` and see:

```
Command line arguments: ['arg1', 'arg2']
```

## Deep Dive

Back in the early command-line days, user interaction was all about text. Linux, UNIX, and Windows used command line args to tell programs what to do.

Now for the alternatives: besides `process.argv`, in Node.js, you could use libraries like `yargs` or `commander` for more features like parsing and validation.

The guts of this in TypeScript are simple: `process.argv` is an array with all the arguments. Index 0 is the path to Node, index 1 is the script path, so real args start from index 2.

## See Also

To explore further, start with these:

- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs GitHub repository](https://github.com/yargs/yargs)
- [Commander.js GitHub repository](https://github.com/tj/commander.js)
