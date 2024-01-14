---
title:    "Swift recipe: Printing debug output"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of software development. As a programmer, it is crucial to be able to identify and fix errors in your code. One useful tool in the debugging process is printing debug output. This allows you to see the values of variables and track the flow of your code. In this blog post, we will take a closer look at printing debug output in Swift.

## How To

To print debug output in Swift, we use the `print()` function. Let's say we have a simple function that calculates the sum of two numbers:

```Swift
func sum(_ a: Int, _ b: Int) -> Int {
  let result = a + b
  print("The sum of \(a) and \(b) is \(result)")
  return result
}
```
In the above code, we use the `print()` function to show the values of the parameters `a` and `b` as well as the result of their sum. To use the `print()` function, we pass in the values we want to print as arguments inside the parentheses. The values are separated by commas and can be of any type, including strings, integers, and variables.

Let's call our `sum()` function and see the debug output in action:

```Swift
let result = sum(2, 3)
```

The output of the above code would be:

```
The sum of 2 and 3 is 5
```

We can also print statements without any variables or values, as shown in the following example:

```Swift
let greeting = "Hello"
print("\(greeting) World!")
```

The output would be:

```
Hello World!
```

## Deep Dive

Now that we know how to use the `print()` function to print debug output, it's essential to understand some of its features.

### Multiple Values

As we saw in the examples above, we can print multiple values using the `print()` function by separating them with commas. We can also use the special separator `terminator` to change the default newline separator. For example, if we want to print multiple values on the same line, we can set the `terminator` to an empty string:

```Swift
print("Hello", "World!", separator: " ", terminator: "")
```

The output would be:

```
Hello World!
```

### Customizing Output

The `print()` function also allows us to format the output by using string interpolation and special characters. For example, we can use `\n` to create a new line in our output string:

```Swift
print("First line\nSecond line\nThird line")
```

The output would be:

```
First line
Second line
Third line
```

### Debug Mode

In Swift, we have a built-in `debugPrint()` function that works similarly to `print()` but is designed specifically for debugging purposes. This function is only called when our code is run in debug mode, making it perfect for debugging output that we don't want to appear in our final product.

## See Also

- [Swift Debugging in Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html)
- [Debugging in Swift](https://learnappmaking.com/debugging-swift-xcode-debugger/)
- [Debugging Your Code with Print Statements in Swift](https://spin.atomicobject.com/2016/06/02/debugging-code-swift/)