---
title:                "TypeScript recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Do you ever find yourself needing to save your data in a structured and easily readable format? Look no further, as writing a text file is the solution for you! With just a few lines of code, you can create a text file to store your information in a clear and organized manner.

## How To

To start, let's create a new TypeScript file. We'll call it "text-file.ts". Next, we'll define a function that will handle writing to our text file. 

```
TypeScript

function writeToFile(fileName: string, data: string) {
    // code to write data to the file
}
```

We'll now use the `fs` module to create our text file and write our data to it. 

```
TypeScript

import * as fs from 'fs';

try {
    // create a new file named "data.txt"
    fs.writeFileSync("data.txt", "This is some sample data.");
} catch (error) {
    console.log("Error creating file: " + error);
}
```

If all goes well, we should now have a text file named "data.txt" in our project folder, with the given data written inside it. Simple, right?

## Deep Dive

But let's dive a little deeper into what just happened. The `fs` module, short for file system, is a built-in module in Node.js that allows us to work with files and directories. We used the `writeFileSync()` method from this module to write data to our text file synchronously, meaning our code will wait until the write operation completes before moving on to the next line. 

You can also use `fs.writeFile()` to write data asynchronously, which means our code will continue executing without waiting for the write operation to finish. However, for simplicity's sake, we opted for the synchronous approach in this example.

Additionally, the file we created will be saved in the same directory as our TypeScript file by default. But we can also specify a different file path or even use the `path` module to handle file paths dynamically.

## See Also

- Official Node.js documentation on `fs` module: https://nodejs.org/api/fs.html
- More in-depth tutorial on working with files in TypeScript: https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-node-js
- Another blog post on creating text files in TypeScript: https://timdeschryver.dev/blog/write-text-files-with-typescript