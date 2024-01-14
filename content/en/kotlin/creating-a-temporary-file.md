---
title:                "Kotlin recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
Creating temporary files is a common practice in many programming languages, including Kotlin. Temporary files are useful for storing data that is only needed for a limited period of time. They can also be used for testing and debugging purposes. 

## How To
To create a temporary file in Kotlin, we can use the `createTempFile()` function from the `File` class. Let's take a look at an example:

```Kotlin
val tempFile = File.createTempFile("data", ".txt") // creates a temporary file with "data" as prefix and ".txt" as suffix
tempFile.writeText("This is a temporary file!") // writes text to the file
println(tempFile.absolutePath) // prints the absolute file path
```

The output of the above code would be something like this: 
`/var/folders/gz/5rlr2wls0bv1ygc5b07d9kfc0000gp/T/data7740775868298776485.txt`

As you can see, the temporary file is stored in the `T` folder, which is the default location for temporary files on most operating systems.

## Deep Dive
Now, let's take a deeper look at how the `createTempFile()` function works. This function has two parameters: `prefix` and `suffix`. The prefix is used to give the temporary file a meaningful name, while the suffix determines the file extension. If you don't specify a prefix or suffix, a random name will be generated for the file.

By default, the temporary file is created in the default temporary file directory of the operating system. However, you can also specify a different directory by passing a `directory` parameter to the `createTempFile()` function.

Once the temporary file is created, it can be used like any other file. You can read from it, write to it, and even delete it once you no longer need it. The file will be automatically deleted when the program terminates, but you can also delete it manually by calling the `delete()` function.

## See Also
- [JavaDocs for `File.createTempFile()`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Kotlin Docs for `File` class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html#create-temp-file)

Creating temporary files in Kotlin is a simple and useful task. Whether you need to store data temporarily or just want to practice your coding skills, this feature comes in handy. Make sure to check out the links above for more information and ways to use temporary files in Kotlin. Happy coding!