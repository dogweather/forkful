---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Swift programming means creating the foundation for a wholesome application, essentially from scratch. Programmers embark on new projects to bring fresh ideas to life, solve problems, or innovate within tech's ever-evolving domain.

## How To: Start a New Swift Project 
Get your hands dirty. Here's how you can create a new project in Swift:

1. Open Xcode and hit "Create a new Xcode project".
2. Select "iOS" then "App", hit next.
3. Give it a name, select Swift as the language, click "Next".
4. Choose where you want to save it, and press "Create".

```Swift
// HelloWorld.swift 
import Swift
print("Hello, World!")
```

Run this program, and you'll see:

```
Hello, World!
```

And that’s it. You’ve created and executed your first Swift program.

## Deep Dive

Creating a new project may appear simple, yet, it's the foundation of every application you'll ever build. The concept has roots in the inception of software development, with analogs in nearly every language: Python with `__init__.py`, Java with its projects in Eclipse, etc.

Alternatives to starting a project in Xcode exist. You can spin up Swift in a Docker container or use an online Swift playground. Yet, Xcode remains the go-to for iOS-specific projects. 

When you create a new Swift project in Xcode, under the hood, it's doing a lot more than creating a .swift file. It sets up the project infrastructure including the arrangement of directories, links to frameworks and libraries, and the necessary build settings.

## See Also

For further reading and exploration, check these out:

- Apple’s Swift Programming Language Guide: https://docs.swift.org/swift-book/
- Ray Wenderlich tutorials, a great resource for Swift and iOS development in general: https://www.raywenderlich.com/ios
- Xcode, the fundamental tool for iOS App Development: https://developer.apple.com/xcode/
- Using Docker with Swift: https://www.docker.com/what-docker