---
title:                "TypeScript recipe: Starting a new project"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new project can seem like a daunting task for any programmer, but with the rise of TypeScript, it has become more accessible than ever. By using TypeScript, you can easily build scalable and maintainable applications with the help of strong typing and advanced JavaScript features. In this blog post, we will explore the benefits of starting a new project in TypeScript and provide you with a step-by-step guide on how to get started.

## How To
To start a new project in TypeScript, follow these simple steps:

1. Install TypeScript globally on your system by running the command `npm install -g typescript`.
2. Create a new folder for your project and navigate to it in your terminal.
3. Initialize your project with `npm init -y` to create a `package.json` file.
4. Install the required development dependencies by running `npm install --save-dev typescript ts-loader webpack`.
5. Create a `tsconfig.json` file by running the command `tsc --init`.
6. In your `tsconfig.json` file, specify the entry point of your project in the `rootDir` property and the output location in the `outDir` property.
7. Create a `webpack.config.js` file and add the following code:

```
const path = require('path');

module.exports = {
  mode: 'development',
  entry: './src/index.ts',
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
  },
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist'),
  },
};
```

8. Create a `src` folder and add an `index.ts` file. This will be the entry point for your project.
9. Write your TypeScript code in the `index.ts` file, such as:

```
const name: string = "John";
console.log(`Hello ${name}!`);
```

10. Lastly, run the command `npm run build` in your terminal to transpile your TypeScript code into JavaScript and bundle it using webpack. This will create a `bundle.js` file in your `dist` folder.
11. You can now run your project by opening the `index.html` file and viewing the console to see the output.

## Deep Dive
Starting a new project in TypeScript offers many advantages, such as:

- **Reduced errors:** TypeScript's strong typing system helps catch errors during development, making debugging easier.
- **Better code organization:** With the use of interfaces and classes, your code becomes more structured and easier to maintain.
- **Improved scalability:** As your project grows, TypeScript allows for easier refactoring and scaling without compromising the integrity of your code.
- **Support for modern JavaScript features:** TypeScript supports the latest features of JavaScript, making it a powerful tool for building modern applications.

In addition to these benefits, TypeScript also has a large and active community, with great documentation and support available. It also integrates well with popular frameworks like React and Angular, making it a versatile choice for any project.

## See Also
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [React with TypeScript](https://create-react-app.dev/docs/adding-typescript/)
- [Angular with TypeScript](https://angular.io/guide/typescript-configuration)