---
date: 2024-01-20 17:56:47.762214-07:00
description: "Reading command line arguments lets your Swift program grab extra details\
  \ when users run it. This matters because it adds customizability and control\u2026"
lastmod: '2024-02-25T18:49:56.845661-07:00'
model: gpt-4-1106-preview
summary: "Reading command line arguments lets your Swift program grab extra details\
  \ when users run it. This matters because it adds customizability and control\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets your Swift program grab extra details when users run it. This matters because it adds customizability and control without the need for user interaction while the program is running.

## How to:

Swift makes reading command line arguments super straightforward. They're accessible through the `CommandLine` structure. Here's the gist:

```swift
for argument in CommandLine.arguments {
    print(argument)
}
```

If you throw this into a `main.swift` file and run your program with some extra text, like `swift run YourProgram foo bar`, your output will look like this:

```
/path/to/YourProgram
foo
bar
```

That's each argument printed out, including the path to your program as the first element – always keep that in mind!

## Deep Dive

Historically, command line arguments have been a staple in programming, letting folks tailor a program's behavior without changing code. It's Unix's legacy, and pretty much all languages support this feature.

In Swift, `CommandLine.arguments` is an array of strings, with each element being a slice of your input, split by whitespace. This array is handed off by the operating system when your program starts; Swift just makes it easy to access.

Besides `CommandLine.arguments`, you could dive into more complex parsing with libraries like `Swift Argument Parser` for more heavy lifting. This is handy for when you need more than just simple inputs – think flags, options, and sub-commands.

Implementation-wise, those command line arguments get to you through a C array behind the scenes – good old `argc` and `argv`. Swift keeps it hidden but still retains the same basic behavior you'd find in C or C++.

## See Also

- For a broad look at command line programs in Swift, check out the [Swift.org Documentation](https://swift.org/getting-started/#using-the-package-manager).
- To up your argument parsing game, go see the [Swift Argument Parser GitHub repo](https://github.com/apple/swift-argument-parser) for more sophisticated setups.
- If you're curious how other languages handle this, try comparing this with Python's `sys.argv` or Node's `process.argv`.
