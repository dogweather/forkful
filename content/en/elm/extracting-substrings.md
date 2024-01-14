---
title:    "Elm recipe: Extracting substrings"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to extract a specific part of a string in your Elm program? Maybe you need to get the first few characters, or isolate a word in the middle. The ability to extract substrings can come in handy in many different scenarios.

## How To

Extracting substrings in Elm is quite simple. Let's say we have the following string: 

```Elm
let str = "Hello, world!"
```

To get the first five characters, we can use the `String.left` function:

```Elm
String.left 5 str
```

This would output "Hello". Similarly, to get the last four characters, we can use `String.right`:

```Elm
String.right 4 str
```

This would output "ld!". We can also extract a specific range of characters using `String.slice`, which takes in a starting index and an ending index:

```Elm
String.slice 7 11 str
```

This would output "worl".

## Deep Dive

Behind the scenes, Elm's string functions use the underlying JavaScript methods, which means they adhere to the Unicode standard. This ensures that string operations are consistent and reliable, regardless of the characters used in the string. It's important to note that the starting index of a string in Elm is always 0, unlike some other languages where it may be 1.

Another useful function for extracting substrings is `String.fromInt`, which can be used to convert an integer value to a string:

```Elm
String.fromInt 1234
```

This would output "1234". You can even add optional padding to the string if needed:

```Elm
String.fromInt (String.padLeft 6 '0' 123)
```

This would output "000123".

## See Also

- [Elm String documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [JavaScript String methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)