---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debugging Magic in Swift: An Introduction to Print Debugging

## What & Why?
"Print" debugging is the act of outputting variable states and program flow info to the console. It's like a debugging GPS that helps locate and fix bugs, offering real-time insights without breakpoints.

## How to?

Straight to the action, let's write out the classic greeting to the world:

```Swift
print("Hello, World!")
```

The output? Expect to see:

```
Hello, World!
```

Fantastic? Let's kick it up a notch. Printed output can include variable values:

```Swift
let greeting = "Howdy, Mars!"
print(greeting)
```

You'll shake hands with:

```
Howdy, Mars!
```

Want something more advanced? Go for string interpolation:

```Swift
let planet = "Mars"
print("Howdy, \(planet)!")
```

This will yield:

```
Howdy, Mars!
```

## Deep Dive

Back in the day, print debugging has been THE strategy, especially when transparent debuggers weren't commonplace yet. 

Consider "print()" equivalents. Say "debugPrint()" for complex data types:

```Swift
let alien = ["name": "Marvin", "planet": "Mars"]
debugPrint(alien)
```

This will unmask:

```
["name": "Marvin", "planet": "Mars"]
```

We also have the "dump()" function, which provides a more detailed, hierarchy view of objects:

```Swift
let spaceship = ["type": "UFO", "capacity": 3]
dump(spaceship)
```

and behold:

```
▿ 2 elements
  - key: "type"
  - value: "UFO"
  - key: "capacity"
  - value: 3
```

These are integral for debugging complex structures. But remember, scrub prints before shipping your code. You wouldn't want your secret debug comments exposed, would you?

## See Also

Hungry for more? Binge on these resources:

1. [Apple's official documentation on "print"](https://developer.apple.com/documentation/swift/1541053-print)
2. [A StackOverflow thread on "print vs debugPrint"](https://stackoverflow.com/questions/25951195/swift-print-vs-debugprint)
3. [Explore "dump" function in depth](https://www.hackingwithswift.com/example-code/language/what-is-the-dump-function)