---
title:    "Swift recipe: Printing debug output"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 
Debug output is an essential tool for any programmer during the development process. It allows you to track the flow of your code, identify errors and troubleshoot issues more efficiently. And with Swift's print() function, printing debug output has never been easier. 

## How To 

To print debug output in Swift, we use the print() function. This function takes in a value or variable as an argument and prints it to the console. Let's take a look at a simple example: 

```Swift 
let message = "Hello, world!"
print(message) 
```

In this example, we have declared a constant named "message" with a value of "Hello, world!" and then printed it using the print() function. When we run this code, the output in the console will be "Hello, world!". 

We can also print multiple values or variables in a single statement by separating them with a comma. For example: 

```Swift 
let name = "John"
let age = 27
print("My name is \(name) and I am \(age) years old.") 
```

This will print out "My name is John and I am 27 years old." to the console. As you can see, we can also use string interpolation to include the values of our variables in the printed statement. 

## Deep Dive 

There are a few more things to consider when it comes to printing debug output in Swift. Firstly, we can use the terminator and separator parameters to customize the output. The terminator parameter specifies what should be printed after the value, and the separator parameter specifies what should be printed between each value. By default, both of these parameters are set to "\n" (new line). 

We can also use the debugPrint() function, which prints a more detailed description of the value or variable we pass in. This can be useful when debugging more complex data types. 

Lastly, it's important to note that using print() for debug output can affect the performance of our code. This is because it has to convert the value or variable to a string before printing it. So it's best practice to remove any debug statements from our code before publishing it to production. 

## See Also 

To learn more about the print() and debugPrint() functions, check out the official Apple documentation: 

- [print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- [debugPrint(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541130-debugprint)