---
title:                "TypeScript recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new programming project can seem daunting, but it can also be exciting! With the power of TypeScript, you can create robust and scalable applications while also enjoying the perks of a modern, typed language. In this blog post, we'll explore the benefits of using TypeScript and how to get started with your own project.

## How To

To get started with TypeScript, you'll need to have Node.js installed on your computer. Once that's taken care of, you can use the Node Package Manager (npm) to install TypeScript globally:

```TypeScript
npm install -g typescript
```

Next, create a new directory for your project and navigate into it. Then, run the following command to initialize a new TypeScript project:

```TypeScript
tsc --init
```

This will create a `tsconfig.json` file, which is used to configure your project's TypeScript settings.

Now, you can start writing your code! Here's a simple "Hello World" example in TypeScript:

```TypeScript
// main.ts

const greeting: string = "Hello World!";
console.log(greeting);
```

In the above code, we define a variable `greeting` with the type `string` and assign it the value of "Hello World!" Then, we use `console.log()` to print the greeting to the console. To compile and run this code, use the following command:

```TypeScript
tsc main.ts && node main.js
```

You should see "Hello World!" printed to your console. Congrats, you've written your first TypeScript code!

## Deep Dive

In addition to its strong typing system, TypeScript also offers a wide range of features such as interfaces, classes, and decorators. These features make it easier to write and maintain complex code. TypeScript also allows for better error checking and code completion, making debugging much easier.

For a deeper dive into TypeScript, you can explore the official documentation and experiment with more advanced features. You can also look into building TypeScript projects using popular frameworks like Angular or React.

See Also: 

- Official TypeScript documentation: https://www.typescriptlang.org/docs/
- Angular: https://angular.io/
- React: https://reactjs.org/