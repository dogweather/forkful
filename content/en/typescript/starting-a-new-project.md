---
title:                "Starting a new project"
html_title:           "TypeScript recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project can be a daunting task, but with the rising popularity of TypeScript, it has become an enticing option for many developers. With its strong type system and powerful features, TypeScript offers a more structured approach to JavaScript, making it a worthy choice for any new project.

## How To

To start a new TypeScript project, first make sure you have TypeScript installed globally on your machine. You can do this by running `npm install -g typescript` in your terminal.

Next, create a new folder for your project and initialize it as a Node.js project with `npm init -y`. This will create a `package.json` file in your project folder.

Now, create a new file called `index.ts` and let's write our first TypeScript code:

```TypeScript
// index.ts
const message: string = "Hello, TypeScript!";
console.log(message);
```

Here, we declare a variable `message` with a type annotation of `string`, and then log it to the console. To compile this code into JavaScript, run command `tsc index.ts` in your terminal. This will create a `index.js` file in the same folder.

To run our code, we need to use `node index.js` in the terminal. You should see the message "Hello, TypeScript!" printed in the console.

## Deep Dive

Now, let's dive deeper into starting a new TypeScript project. One of the first steps is to set up our `tsconfig.json` file, which is a configuration file for TypeScript projects. This file tells the TypeScript compiler how to compile our TypeScript code into JavaScript.

To generate a basic `tsconfig.json` file, run command `tsc --init` in your terminal. This will create a file with default settings, but you can customize it according to your project's needs. Some common configurations include specifying the version of TypeScript, target environment, and output directory.

Another important aspect of a TypeScript project is using type definitions. These are files that describe the types and structures of external JavaScript libraries or modules, allowing TypeScript to understand and use them in your code.

You can install type definitions for a specific library using `npm install @types/library-name`. For example, if you're using Express in your project, you can install its type definitions using `npm install @types/express`. These type definitions will be automatically included in your `tsconfig.json` file under the `types` section.

## See Also

- Official TypeScript documentation: https://www.typescriptlang.org/docs/home.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Learn TypeScript in 5 minutes: https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html
- Setting up a TypeScript project: https://www.typescriptlang.org/docs/handbook/setting-up-your-project.html