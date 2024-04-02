---
date: 2024-01-20 18:04:27.083983-07:00
description: "Starting a new project in TypeScript is all about setting up a solid\
  \ foundation to code on. Programmers initiate new projects to turn fresh ideas into\u2026"
lastmod: '2024-03-13T22:44:59.859527-06:00'
model: gpt-4-1106-preview
summary: "Starting a new project in TypeScript is all about setting up a solid foundation\
  \ to code on. Programmers initiate new projects to turn fresh ideas into\u2026"
title: Starting a new project
weight: 1
---

## What & Why?
Starting a new project in TypeScript is all about setting up a solid foundation to code on. Programmers initiate new projects to turn fresh ideas into working software, test out concepts, or learn new things.

## How to:
```TypeScript
// Step 1: Install TypeScript globally (if not installed)
npm install -g typescript

// Step 2: Create a new directory for your project
mkdir my-new-project
cd my-new-project

// Step 3: Initialize a new node project
npm init -y

// Step 4: Install TypeScript in your project
npm install typescript --save-dev

// Step 5: Initialize a TypeScript project to create tsconfig.json
tsc --init

// Sample tsconfig.json output (with some fields omitted for brevity)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// Step 6: Create a simple TypeScript file 'hello.ts'
echo 'console.log("Hello, TypeScript!");' > hello.ts

// Step 7: Compile the TypeScript file and run it
tsc hello.ts
node hello.js

// Sample output
Hello, TypeScript!
```

## Deep Dive
TypeScript, a superset of JavaScript, was developed by Microsoft and first released in October 2012. It adds static types to JavaScript, which can help catch errors before runtime and support IDE features like code navigation and refactoring.

While the above procedure uses npm (Node Package Manager), there are other ways to manage TypeScript projects, like Yarn or pnpm. Alternatives to initiating a TypeScript project include creating a project using a starter kit or cloning a boilerplate from repositories like GitHub.

The `tsconfig.json` is crucial; it guides how the TypeScript Compiler (tsc) converts your TypeScript code to JavaScript. Tweaking compiler options lets you target different ECMAScript versions, module systems, and more, tailoring to your project needs.

## See Also
- TypeScript Official Docs: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- TypeScript GitHub Repo: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- TypeScript Deep Dive: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- Awesome TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)
