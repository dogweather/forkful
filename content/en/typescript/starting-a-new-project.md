---
title:    "TypeScript recipe: Starting a new project"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why: Starting a New TypeScript Project

As a developer, starting a new project can be both exciting and daunting. Whether you're a beginner or an experienced programmer, starting a new TypeScript project can open up a world of possibilities for your coding journey. TypeScript is a powerful programming language that offers a safe and structured way of developing large-scale applications. In this blog post, we'll explore why you should consider using TypeScript for your next project and how to get started.

## How To: Setting up a TypeScript Project

To get started with TypeScript, you'll need to have Node.js and npm installed on your computer. Once you have those set up, you can follow these steps to create a new TypeScript project:

1. Open up your terminal or command prompt and navigate to the directory where you want to create your project.
2. Run the command `npm init` to initialize a new npm project. This will create a `package.json` file in your directory.
3. Next, install TypeScript by running the command `npm install typescript --save-dev`. This will install TypeScript as a dev dependency in your project.
4. Create a new file named `tsconfig.json` in your project directory. This file will serve as the configuration for TypeScript in your project.
5. Add the following code to your `tsconfig.json` file:

```TypeScript
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    "strictNullChecks": true,
    "esModuleInterop": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist"
  },
  "include": [
    "src/**/*"
  ]
}
```

This code sets up some basic compiler options for TypeScript, such as the target JavaScript version and the location of the compiled code. It also includes the `src` directory as the place where TypeScript will look for your source code.

6. Now, create a new `src` directory and add a `index.ts` file inside it. This will be the entry point for your TypeScript project.
7. Finally, add some code to your `index.ts` file, such as:

```TypeScript
console.log("Hello, world!");
```

8. To compile your TypeScript code, run the command `tsc` in your terminal. This will create a `dist` directory in your project, which will contain the compiled JavaScript code.
9. To see the output of your code, run the command `node dist/index.js`. You should see the message "Hello, world!" printed in your terminal.

Congratulations, you have successfully set up a new TypeScript project!

## Deep Dive: Why Choose TypeScript?

TypeScript offers many advantages for developing large-scale applications. One of the main benefits is its static typing system, which allows for better error detection and improved code quality. The type annotations in TypeScript help to catch potential bugs during compilation, making it easier to maintain and scale your codebase.

Additionally, TypeScript is a superset of JavaScript, which means that all JavaScript code is also valid TypeScript code. This makes it easy to integrate TypeScript into existing JavaScript projects or to gradually add it to a codebase.

Another advantage of using TypeScript is its language features, such as classes, interfaces, and generics, which can make code more organized and easier to read. It also has great tooling support, with features like code completion and type definitions available in popular code editors.

## See Also

- [TypeScript Official Website](https://www.typescriptlang.org/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Getting Started with TypeScript on Node.js](https://blog.logrocket.com/getting-started-with-typescript-on-node-js-a6faa72620e4/)
- [10 Reasons to Use TypeScript](https://dabit3.medium.com/10-reasons-to-use-typescript-292c2638afda)

Happy coding!