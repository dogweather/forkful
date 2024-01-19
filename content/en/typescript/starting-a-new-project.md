---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is about setting up a fresh coding environment for a new software/product development. Programmers do it to create a focused, compartmentalized workspace which is isolated from their other projects.

## How to:

Starting a new TypeScript project is straightforward. First, ensure that Node.js and npm (Node package manager) are installed in your system. If not, download them from their respective official sites.

Once done, kickstart your TypeScript project by running the below commands:

```TypeScript
// Create a new directory for your project
mkdir my-typescript-project
cd my-typescript-project

// Initialize a new npm project
npm init -y 

// Install TypeScript as a dev dependency
npm install typescript --save-dev 

//Create a typescript configuration file
npx tsc --init 
```

This will create a tsconfig.json file, which specifies root files and compiler options required to compile the project.

## Deep Dive:

Historically, JavaScript was used for both client-side and server-side development. However, as applications grew complex, developers sought a tool that could assist them in managing larger codebases. TypeScript was introduced by Microsoft in 2012 as an answer to this need. 

Though other alternatives such as Babel can be used to transpile ES6+ code into plain old JavaScript, TypeScript remains a popular choice due to its static typing and OOP features, which increase code predictability and maintainability.

When we initialize a TypeScript project, `tsc --init` command creates a tsconfig.json with a host of compiler flags. These flags control various aspects of TypeScript, such as enforcing strict type checking (`"strict": true`) or the ECMAScript target level (`"target": "es5"`). Modify these flags according to your project requirements.

## See Also:

- An in-depth guide to TypeScript: https://www.typescriptlang.org/docs/ 
- Node.js official download page: https://nodejs.org/en/download/ 
- npm official site: https://www.npmjs.com/
- More on TypeScript compiler flags: https://www.typescriptlang.org/tsconfig
- Babel - a JavaScript compiler worth considering : https://babeljs.io/