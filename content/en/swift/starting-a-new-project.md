---
title:                "Swift recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project can be both exciting and daunting. You may have a great idea in mind or a business problem that needs to be solved. Whatever the reason, beginning a new programming project is a thrilling opportunity to bring your ideas to life.

## How To

To get started, you'll need to have Xcode installed on your Mac. If you're new to Swift, don't worry! It's a beginner-friendly programming language with a simple syntax. Let's dive into some examples using Swift playgrounds.

### Setting Up a Blank Project

First, let's open Xcode and create a new project. Select "Single View App" and click "Next". Name your project and select "Swift" as the language and make sure you have "Storyboard" selected for User Interface. Click "Next" and save your project. Congratulations, you've just set up a blank Swift project!

### Declaring Variables

Next, let's learn how to declare variables in Swift. Variables in Swift can hold different types of data, such as strings, integers, and booleans. To declare a variable, use the `var` keyword followed by the variable name and the data type. Let's see this in action:

```Swift
var projectName: String = "My New Project"
var numberOfTasks: Int = 10
var isComplete: Bool = false
```

### Creating Functions

Functions are blocks of code that perform a specific task. In Swift, you can create a function by using the `func` keyword followed by the function name and its parameters within parentheses. Let's create a simple function that prints a message to the console:

```Swift
func printMessage(message: String) {
  print(message)
}

printMessage("Welcome to my new project!")
```

### Running Your Code

To see the output of your code, click the "Run" button on the top left of Xcode. You should see your message printed in the console area at the bottom of the Xcode window. Congratulations, you've successfully run your first Swift project!

## Deep Dive

Starting a new project can feel overwhelming, but don't let that discourage you. Take some time to plan out your project and break it down into smaller tasks. This will help you stay organized and motivated. You can also make use of official Swift documentation and online resources to learn more about the language and its features.

## See Also

- [Official Swift Documentation](https://swift.org/documentation/)
- [Swift Playgrounds](https://www.apple.com/swift/playgrounds/)
- [Swift Community on GitHub](https://github.com/apple/swift-community-hosted-projects)