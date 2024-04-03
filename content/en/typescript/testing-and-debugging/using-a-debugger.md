---
date: 2024-01-25 20:50:44.852715-07:00
description: "To get cracking with a debugger in TypeScript, all you need is a supported\
  \ IDE (like Visual Studio Code) and a `launch.json` configuration. Here's a quick\u2026"
lastmod: '2024-03-13T22:44:59.863012-06:00'
model: gpt-4-1106-preview
summary: To get cracking with a debugger in TypeScript, all you need is a supported
  IDE (like Visual Studio Code) and a `launch.
title: Using a debugger
weight: 35
---

## How to:
To get cracking with a debugger in TypeScript, all you need is a supported IDE (like Visual Studio Code) and a `launch.json` configuration. Here's a quick example for a Node.js application:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

To debug this, create a `launch.json` file under the `.vscode` folder:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Launch Program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Then, set a breakpoint in your `greet` function by clicking on the left-hand side of the line number in your IDE. Hit F5 to start debugging, and watch your app pause at the breakpoint. You can now hover over variables, watch expressions, and step through your code with ease.

## Deep Dive
Back in the day before integrated development environments (IDEs) got slick, debugging was often done with print statements (a.k.a `console.log` debugging). It worked, kind of, but was like trying to find a needle in a haystack blindfolded.

Modern debuggers are like a Swiss Army knife for troubleshooting. With the evolution of TypeScript and Node.js, there are various debuggers available, from the built-in Node.js inspector to browser dev tools for client-side debugging.

The Node.js inspector works by attaching to your running application; it communicates over the Chrome DevTools Protocol, turning your Chrome browser into a mighty debugging console. This integration allows for a visually interactive and detailed debugging session, compared to traditional command-line debugging practices.

## See Also
For a little extra reading and some pro-tips, check out:

- [TypeScript Debugging in Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools Documentation](https://developers.google.com/web/tools/chrome-devtools)
