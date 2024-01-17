---
title:                "Interpolating a string"
html_title:           "Elm recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string in Elm refers to the process of inserting values into a string at runtime. This allows programmers to dynamically create strings with variable data, making code more efficient and flexible.

## How to:

Interpolating a string in Elm is simple. All you have to do is use the `toString` function and the `++` operator. Take a look at the example below:

```
import Html exposing (text)

-- Define a variable with the value we want to insert
variable = 5

-- Interpolate the string with the variable
myString = "The value of the variable is" ++ (toString variable)

-- Display the interpolated string on the screen
main =
    text myString
```

The output of this code will be:

```
The value of the variable is 5
```

You can also interpolate multiple variables in a string by using the `++` operator multiple times. For example:

```
import Html exposing (text)

-- Define variables with the values we want to insert
name = "John"
age = 30

-- Interpolate the string with the variables
myString = "My name is " ++ name ++ " and I am " ++ (toString age) ++ " years old."

-- Display the interpolated string on the screen
main =
    text myString
```

The output of this code will be:

```
My name is John and I am 30 years old.
```

## Deep Dive

Interpolating strings is a popular practice in programming languages as it allows for more dynamic and flexible code. It is particularly useful when dealing with user input, where values may change and need to be displayed in a string.

In Elm, there are alternatives to interpolating strings, such as using the `concat` function or using the `Text.format` module. However, interpolating strings with `toString` and `++` remains the preferred method for its simplicity and readability.

When interpolating strings in Elm, it is important to keep in mind that the `toString` function can only convert basic types like integers and strings. Complex types like records or custom types will need to be converted first using the `Debug.toString` function.

## See Also

To learn more about string interpolation in Elm, check out the official documentation: https://guide.elm-lang.org/interop/strings.html

You can also explore other string manipulation functions in Elm, such as `String.concat` and `String.join` here: https://package.elm-lang.org/packages/elm/core/latest/String#concat