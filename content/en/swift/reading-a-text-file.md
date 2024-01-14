---
title:                "Swift recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a common way to store and share data between programs. Whether you're working with configuration files or user input, the ability to read and manipulate text files is an essential skill for any programmer.

## How To

Reading a text file in Swift is a simple and straightforward process. Here's an example of how you could read a file line by line and print out each line:

```Swift
// Open file for reading
if let fileURL = Bundle.main.url(forResource: "myFile", withExtension: "txt") {
    
    do {
        // Read the file contents
        let fileContents = try String(contentsOf: fileURL, encoding: .utf8)
        
        // Split contents into an array of lines
        let lines = fileContents.components(separatedBy: "\n")
        
        // Loop through lines and print them out
        for line in lines {
            print(line)
        }
    } catch {
        // Handle error
        print(error)
    }
} else {
    print("File not found")
}
```

This code uses the `Bundle` class to locate the text file, then uses the `String` class to read its contents. By using the `components(separatedBy: \n)` function, we can split the text into individual lines. Finally, we loop through each line and print it out to the console. 

Another useful way to read a text file is to use the `Scanner` class, which allows you to read the file using different delimiters or patterns. Let's take a look at an example:

```Swift
// Open file for reading
if let fileURL = Bundle.main.url(forResource: "myFile", withExtension: "txt") {
    
    do {
        // Create a scanner from the file URL
        let scanner = Scanner(url: fileURL)
        
        // Set the delimiter to a comma
        scanner.charactersToBeSkipped = CharacterSet(charactersIn: ",")
        
        // Loop through and print the content
        while let content = scanner.readComponents(separatedBy: ",") {
            print(content)
        }
        
    } catch {
        // Handle error
        print(error)
    }
} else {
    print("File not found")
}
```

In this code, we create a `Scanner` object from the file URL and set the delimiter to a comma. Then, using the `readComponents(separatedBy: )` function, we can read and print each component based on the delimiter we set.

## Deep Dive

Now that we've seen some examples of how to read a text file, let's dive a bit deeper into the process. When working with text files, it's important to understand what encoding is being used. Encoding is the process of converting characters into bytes so that they can be stored or transmitted. If you're not using the correct encoding, you may end up with garbled or incorrect data.

In Swift, the `String` class has an `encoding` property that allows you to specify the encoding when reading a file. By default, it is set to `.utf8` which is a commonly used encoding. However, depending on the source of the text file, you may need to use a different encoding such as `.ascii` or `.unicode`.

Another important aspect to keep in mind is error handling when reading a text file. The `String` class's `init(contentsOf: encoding: )` function throws an error, so it's good practice to wrap it in a `do-catch` block as seen in our examples. This allows you to handle any possible errors that may occur, such as the file not existing or the encoding being incorrect.

## See Also

For more information on reading text files in Swift, check out these helpful resources:

1. [Apple's official documentation on the String class](https://developer.apple.com/documentation/swift/string)
2. [A guide to working with text files in Swift](https://www.hackingwithswift.com/read/32/overview)
3. [A tutorial on using the Scanner class](https://www.raywenderlich.com/12253209-working-with-files-in-swift-5-1/lessons/5)