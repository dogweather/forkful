---
title:    "Swift recipe: Writing to standard error"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

There are many reasons why a programmer may want to write to standard error in their Swift code. One common reason is to log error messages or debugging information to help troubleshoot issues during development. Another reason may be to display specific feedback or warnings to the user, such as in a command line application.

## How To

Writing to standard error in Swift is a simple process. First, you need to import the Foundation framework in your code:

```Swift
import Foundation
```

Next, you can use the function `NSLog` to print a string to standard error. For example:

```Swift
NSLog("An error occurred during the execution of this function.")
```

The output of this code would be:

```
An error occurred during the execution of this function.
```

You can also include variables or other data in the string by using the `%@` placeholder. For example:

```Swift
let message = "This is a debug message."
NSLog("Debug message: %@", message)
```

The output would be:

```
Debug message: This is a debug message.
```

## Deep Dive

Behind the scenes, the `NSLog` function uses the `stderr` stream to write to standard error. This ensures that the output is directed to the correct location, separate from the standard output.

One important thing to note is that the `NSLog` function includes a timestamp and the process ID in the output. This can be helpful when trying to track down specific errors or issues.

## See Also

For more information on writing to standard error in Swift, check out these resources:

- [Apple's documentation on NSLog](https://developer.apple.com/documentation/foundation/nslog)
- [NSHipster's article on Error Handling in Swift](https://nshipster.com/error-handling-in-swift/)
- [Stack Overflow thread on NSLog alternatives](https://stackoverflow.com/questions/9245716/nslog-alternative-in-swift)