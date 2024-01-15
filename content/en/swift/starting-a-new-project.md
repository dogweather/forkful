---
title:                "Starting a new project"
html_title:           "Swift recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Are you ready to dive into the world of Swift programming? Maybe you have an amazing app idea that you want to bring to life, or you simply want to learn a new programming language. Whatever your reason may be, starting a new project in Swift is an exciting and rewarding experience.

## How To

To start a new project in Swift, follow these simple steps:

1. Open Xcode, the IDE (integrated development environment) for Swift.
2. Click on "Create a new Xcode project" or go to File > New > Project.
3. Choose the type of project you want to create (e.g. iOS app, macOS app, etc.).
4. Enter the necessary information for your project, such as name, organization, and bundle identifier.
5. Choose a location to save your project and click "Create".

Congratulations, you have now created your first Swift project! Now, let's take a closer look at what's inside.

```swift
print("Hello, world!")
```

This simple line of code will print the famous "Hello, world!" message, confirming that your project is up and running. Simple, right?

Now, let's add a bit more code to demonstrate the power of Swift. Inside the `ViewController` class, add the following code:

```swift
var favoriteColor = "blue"
var favoriteNumber = 7
print("My favorite color is \(favoriteColor) and my favorite number is \(favoriteNumber).")
```

You will notice that we can declare variables without specifying their type, as Swift is a type-inferred language. In this case, `favoriteColor` will be inferred as a `String` and `favoriteNumber` as an `Int`. The backslash preceding the parentheses in the `print` statement allows us to include the variables' values in the output. If you run the code, you should see "My favorite color is blue and my favorite number is 7" printed in the console.

## Deep Dive

Now that you have successfully created a new project, let's dive a bit deeper into what you can expect when starting a Swift project. You will notice that Xcode has generated a few files for you, including the `AppDelegate` and `ViewController` files.

The `AppDelegate` file is the entry point for your app and contains important methods for handling the app's life cycle. The `ViewController` file is where you will write most of your code for your app's user interface.

One of the great features of Swift is its strong typing system, which helps prevent errors and improve code readability. You will also notice that Swift uses optionals, which allow for safer handling of data that may be nil. This is just the tip of the iceberg when it comes to the features and advantages of using Swift for your projects.

See Also

- Official Swift Documentation: https://developer.apple.com/documentation/swift
- Xcode Tutorials: https://developer.apple.com/tutorials/swiftui/creating-and-combining-views