---
title:                "TypeScript recipe: Writing a text file"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a mundane task for some, but for programmers, it is a crucial task that helps in organizing and storing data in a readable format. As a TypeScript programmer, you may find yourself in the position of needing to write a text file at some point in your project. It could be for logging purposes, saving user inputs, or even parsing data for later use. Whatever the reason may be, knowing how to write a text file is an essential skill that every programmer should have in their arsenal.

## How To

To write a text file in TypeScript, we can use the `fs` module, which is built-in Node.js and doesn't require any additional installation. We will also need the `writeFileSync` method from this module, which takes in two parameters - the path to the file and the data to be written. Here's a simple example of how we can write a text file using TypeScript:

```TypeScript
import fs from 'fs';

const data = 'This is a sample text that we want to write to our file.';
fs.writeFileSync("myFile.txt", data);
```

In the above code, we first import the `fs` module, and then we create a variable `data` that holds the text we want to write to our file. Next, we use the `writeFileSync` method to write that data to a file named `myFile.txt`. This will create a new file in the same directory as our TypeScript file, and if the file already exists, it will overwrite its contents.

We can also use interpolation to dynamically generate the data we want to write to our file. For example, we can have a template string that contains variables, and then use the `replace` function to replace those variables with their corresponding values. Here's an example:

```TypeScript
import fs from 'fs';

const firstName = 'John';
const lastName = 'Doe';

const template = `My name is {{firstName}} {{lastName}}.`;
const data = template.replace('{{firstName}}', firstName).replace('{{lastName}}', lastName);

fs.writeFileSync("myFile.txt", data);
```

The above code will write the following text to our file:

```
My name is John Doe.
```

## Deep Dive

As we saw in the previous section, we can use the `fs` module to write text files in TypeScript. However, there are a few other things we should keep in mind while doing so. 

Firstly, we can specify the encoding of our file while writing it. By default, the `writeFileSync` method uses `'utf-8'` encoding, but we can change it by passing a third parameter to the method. For example, if we want to use `'ascii'` encoding, our code would look like this:

```TypeScript
import fs from 'fs';

const data = 'This is a sample text that we want to write to our file.';
fs.writeFileSync("myFile.txt", data, 'ascii');
```

Secondly, we can specify options for our file while writing it. These options can include things like file permissions, flags, and more. Again, we can pass these options as a third parameter to the `writeFileSync` method. Here's an example:

```TypeScript
import fs from 'fs';

const data = 'This is a sample text that we want to write to our file.';
const options = { mode: 0o755, flag: 'w' };
fs.writeFileSync("myFile.txt", data, options);
```

For a detailed list of options, you can refer to the official Node.js documentation for the `fs` module.

## See Also

- [Node.js Documentation for the `fs` Module](https://nodejs.org/api/fs.html)
- [TypeScript Documentation for the `writeFileSync` Method](https://www.typescriptlang.org/docs/handbook/fs.html#writing-files)