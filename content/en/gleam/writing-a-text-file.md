---
title:                "Gleam recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing text files is an essential skill for any developer, as it allows you to store and manipulate data in a simple and portable format. Whether you're building a web application or automating tasks, understanding how to write text files can greatly enhance your programming capabilities.

## How To

To write a text file in Gleam, you will first need to create a file type definition. This will specify the layout of your text file and the types of data it will contain. For example, if you want to store a list of names, you would define a file type with a `String` field.

```Gleam
type MyTextFile {
  name: String
}
```

Next, you will need to create a function that will write the data to your file. This can be done using the `File.open` function, which takes in the name of the file you want to create and a `File.Mode` which specifies if you want to create a new file or overwrite an existing one. You can then write your data using the `File.write` function.

```Gleam
let names = ["John", "Jane", "Bob"]
let result = File.open("names.txt", File.Write).map(fn(file _) {
  File.write(file, names[0])
})
```

This will create a new file called `names.txt` and write the name "John" to it. If you want to write all the names in your list, you can use a loop or the `File.write_all` function.

```Gleam
let names = ["John", "Jane", "Bob"]
let result = File.open("names.txt", File.Write).map(fn(file _) {
  File.write_all(file, names)
})
```

## Deep Dive

There are many other ways to customize and manipulate your text file in Gleam. For example, you can specify the encoding of your file using the `File.Encoding` type, or append data to an existing file using `File.Append`. You can also use the `File.close` function to close the file after writing to it, ensuring that it is saved properly.

It's important to note that writing to text files in Gleam is a side-effecting operation, meaning that it changes the state of your system. In order to maintain functional purity, it's best to handle errors and results using the `Result` type.

## See Also

- [Gleam official documentation](https://gleam.run/documentation/)
- [Writing Data to Files in Gleam](https://medium.com/gleam-blog/writing-data-to-files-in-gleam-5004b34fdd4a)
- [Manipulating Text Files in Gleam](https://blog.cloudless.studio/posts/manipulating-text-files-in-gleam/)