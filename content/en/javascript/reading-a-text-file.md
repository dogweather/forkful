---
title:                "Javascript recipe: Reading a text file"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to extract information from a text file using Javascript? Maybe you needed to collect data from a log file or parse a CSV file. Whatever the reason may be, learning how to read a text file in Javascript can be extremely useful for data manipulation and analysis.

## How To

Reading a text file in Javascript involves a few simple steps. First, we need to load the file using the `fs` module, which is a built-in module in Node.js that allows us to work with the file system. We can use the `readFile()` method to load the file and specify the encoding as`"utf-8"`.

```
const fs = require('fs'); // load the fs module
const filepath = "example.txt"; // replace with your file path
fs.readFile(filepath, "utf-8", (err, data) => {
   // code to process the file data goes here
});
```

Next, we can use the `split()` method to split the data into an array of lines. This will allow us to access each line individually and work with the data.

```
const lines = data.split('\n'); // split the data into an array of lines
```

From here, we can use a loop to iterate through each line and perform any necessary operations. For example, if our text file contains a list of numbers, we can convert them to integers using the `parseInt()` method.

```
for(i = 0; i < lines.length; i++) {
   const number = parseInt(lines[i]);
   // code to work with the number goes here
}
```
Finally, we can print the output or save it to another file using the `console.log()` or `fs.writeFile()` methods, respectively.

## Deep Dive

The `fs` module also has other methods that can be useful for reading text files. For example, the `readFileSync()` method can be used for synchronous reading, which is useful for smaller files or for simpler scripts. Additionally, we can specify the character encoding of our text file when using the `readFile()` method. Depending on the original encoding of the file, this can be useful for ensuring the correct characters are displayed.

When working with CSV or other structured text files, we can use libraries like `csv-parser` or `papaparse` to parse and manipulate the data more easily. These libraries have built-in functions for reading and parsing CSV files, making it a more efficient and convenient option.

## See Also

- [Node.js `fs` Module Documentation](https://nodejs.org/api/fs.html)
- [CSV-Parser Library](https://www.npmjs.com/package/csv-parser)
- [Papaparse Documentation](https://www.papaparse.com/)