---
title:    "Javascript recipe: Writing a text file"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, there are many different tasks that developers may need to accomplish. One of these tasks is writing a text file. This may not seem like a particularly exciting or important task, but it is a fundamental skill that can come in handy in various situations.

At its core, writing a text file involves creating a file that contains plain text, without any formatting or design elements. This makes it a versatile and straightforward way to store and share information. Whether you need to store user data, log application information, or simply create a report, knowing how to write a text file can be a useful skill to have.

## How To

To start writing a text file using Javascript, you first need to create an instance of the `FileWriter` object. This can be done by using the `new` keyword and passing in the name of the file you want to create, along with the desired file mode (read, write, append, etc.).

```Javascript
let file = new FileWriter("myFile.txt", "write");
```

Next, you can use the `write()` method to add content to your file. This method takes in a string of text as its parameter and appends it to the file.

```Javascript
file.write("This is some text that will be written to my file.");
```

You can also use the `close()` method to close the file once you have finished writing to it. This is an important step as it ensures that all the content has been saved properly.

```Javascript
file.close();
```

Once you have closed the file, you can open it again in "read" mode to check the content that was written.

```Javascript
// Reopen file in "read" mode
file = new FileWriter("myFile.txt", "read");

// Read and log contents of file
let contents = file.read();
console.log(contents);
```

Running this code will output the contents of the file to the console, which in this case would be: "This is some text that will be written to my file."

## Deep Dive

While the steps listed above are the basic steps for writing a text file in Javascript, there are a few things to keep in mind while doing so.

First and foremost, when writing to a text file, it is crucial to properly handle errors. This can be done by using try/catch blocks to catch any potential errors that may occur during the writing process.

Additionally, different file modes will have different results when writing to a file. For example, using the "write" mode will overwrite any existing content in the file, while using "append" mode will add the new content to the end of the existing content in the file.

It is also essential to remember to close the file once you are finished writing to it. Not doing so can result in data loss or corrupt files.

## See Also

- [Javascript Document Object Model (DOM)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
- [Working with Files in Javascript](https://www.tutorialspoint.com/javascript/javascript_files.htm)
- [FileWriter API Reference](https://developer.mozilla.org/en-US/docs/Web/API/FileWriter)