---
title:    "Swift recipe: Writing to standard error"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##Why

In the world of Swift programming, it's important to be able to effectively communicate with the user during runtime. One way to do this is through writing to standard error, which allows for messages and errors to be displayed in a command line interface.

##How To

Writing to standard error is a simple process that involves using the "write" method from the "FileHandle" class. First, we need to import "Foundation" and "FileHandle":

```
import Foundation
import FileHandle
```

Next, we can create a constant called "errorOutput" which represents the standard error output stream:

```
let errorOutput = FileHandle.standardError
```

Now, we can use the "write" method to send a message to the standard error output stream. Let's say we want to display an error message:

```
let errorMessage = "Something went wrong"
errorOutput.write(errorMessage.data(using: .utf8)!)
```

In the above code, we are converting our error message to data using UTF-8 encoding and then sending it to the standard error output stream using the "write" method. This will display the message in the command line.

We can also write to standard error using a file handler. Here's an example:

```
let fileURL = URL(fileURLWithPath: "error.log")
let fileHandler = try! FileHandle(forWritingTo: fileURL)
fileHandler.write(errorMessage.data(using: .utf8)!)
```

This will write the error message to a file called "error.log".

##Deep Dive

Standard error is typically used for displaying error messages and other important information that the user needs to know during runtime. Unlike standard output, any messages written to standard error will not be redirected by default. This allows for more efficient debugging and troubleshooting.

It's also important to note that there is a difference between standard error and standard output. Standard error is generally used for displaying errors, while standard output is used for displaying regular output messages.

Some common use cases for writing to standard error include handling exceptions, displaying error messages during system calls, and providing feedback to users about failed processes.

##See Also

- [The Swift Programming Language](https://docs.swift.org/swift-book/)
- [FileHandle Class Reference](https://developer.apple.com/documentation/foundation/filehandle)
- [Error Handling in Swift](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)