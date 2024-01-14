---
title:    "Gleam recipe: Printing debug output"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming journey - it helps us identify and fix errors in our code. However, sometimes it can be difficult to pinpoint where the error is occurring, especially in larger projects. This is where printing debug output comes in handy. By printing out specific values and variables within our code, we can gain a better understanding of how our program is functioning and where there may be a problem.

## How To

To print debug output in Gleam, we can use the `Debug.print` function. This function takes in a value as its argument and prints it to the console when the program is running. Let's take a look at an example:

```
Gleam import Debug

fn main() {
    let name = "Sarah"
    let age = 27
    Debug.print(name)
    Debug.print(age)
}
```

In this code, we have a `main` function that creates two variables - `name` and `age`. Using `Debug.print`, we can print out the values of these variables to the console. So when we run this program, we will see the output:

```
Sarah
27
```

This allows us to see the values of our variables without having to manually check them during runtime. It can also be useful in tracking the flow of our code and identifying any potential errors.

## Deep Dive

Printing debug output is not just limited to simple values and variables. We can also print complex data structures, such as records and lists. For example:

```
Gleam import Debug

type Person(
    name: String,
    age: Int
)

let person = Person("Alex", 35)
Debug.print(person)
```

In this code, we have a `Person` type with two fields - `name` and `age`. We then create a `person` record with the name "Alex" and age 35. By using `Debug.print`, we can see the entire record in our console, including the field names and values:

```
Person(
    name: "Alex",
    age: 35
)
```

This can be incredibly useful in understanding the data we are working with and identifying any potential issues.

## See Also
- [Gleam Debugging Documentation](https://gleam.run/book/tour/debugging.html)
- [Gleam by Example: Printing Debug Output](https://github.com/lpil/gleam-by-example/blob/main/03-repl/guide.md#printing-debug-output)
- [Debugging Basics for Programmers](https://www.freecodecamp.org/news/debugging-basics-for-programmers-a839b25478cf/)