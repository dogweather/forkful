---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:33.615488-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
JavaScript scripts often need input—the kind you pass along when you run them. Reading command line arguments lets a program act on that input, tailoring behavior without changing the code.

## How to: (Як це зробити:)
Here's how you snag those command line arguments in Node.js.

```javascript
// Grab the Node.js process object
const process = require('process');

// Skip the first two elements in the array
const args = process.argv.slice(2);

// Log'em to see what you've got
console.log(args);

// Run this with: node script.js arg1 arg2
```

Sample Output:
```
[ 'arg1', 'arg2' ]
```

Take it up a notch - use a library like `yargs` for convenient parsing:

```javascript
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// Run this with: node script.js --name=yourname --age=30
```

Sample Output:
```
{ name: 'yourname', age: 30 }
```

## Deep Dive (Глибоке Занурення):
Back in the day, command line args were key, even before fancy GUIs. Now, in the JavaScript Node.js world, `process.argv` is still a solid go-to. Alternatives like `yargs` or `commander` simplify things with parsing and validation.

Node.js packs all command-line arguments in `process.argv` as an array; first two elements are path to the node executable and the script file, hence the `.slice(2)`.

Why use libraries?

- They parse options (like `--name=yourname` into `{ name: 'yourname' }`).
- They handle defaults, required arguments, and help messages.
- Cleaner, more readable code.

One thing – these tools are for Node.js, not browser JavaScript. For web apps, you'd typically use URL parameters, not command line.

## See Also (Дивись Також):
Node.js docs for `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv

Yargs Docs: https://yargs.js.org/

Commander GitHub: https://github.com/tj/commander.js

For web app URL parameters, Mozilla has a solid guide: https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams
