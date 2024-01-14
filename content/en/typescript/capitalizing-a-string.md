---
title:    "TypeScript recipe: Capitalizing a string"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing a string is a common task in programming, especially when dealing with user input or manipulating data. By capitalizing a string, we are able to ensure consistency and readability in our code.

## How To
To capitalize a string in TypeScript, we can use the handy `toUpperCase()` method. This method converts all characters in a string to uppercase, making it perfect for our purpose.

Let's see an example:

```TypeScript
const name = "john";
const capitalized = name.toUpperCase();
console.log(capitalized); // Output: JOHN
```

In this example, we first declare a variable named `name` and assign it the value of "john". Then, we use the `toUpperCase()` method on the `name` variable and store the result in a new variable called `capitalized`. Finally, we log the value of `capitalized` to the console, which will output "JOHN" in uppercase.

We can also use the `toUpperCase()` method directly on the string we want to capitalize, without the need for a separate variable:

```TypeScript
console.log("mary".toUpperCase()); // Output: MARY
```

This second example does the same thing as the first one, but in a more streamlined way.

## Deep Dive
It's important to note that the `toUpperCase()` method only works for ASCII characters. That means it will only convert letters from the English alphabet to uppercase, and won't work for special characters or non-English languages. In those cases, we can use libraries or custom functions to achieve the same result.

Another thing to keep in mind is that the `toUpperCase()` method doesn't change the original string it is called on. Instead, it returns a new string with the uppercase characters. This is useful because it allows us to preserve the original string and use it later if needed.

## See Also
Here are some useful resources to learn more about strings and string manipulation in TypeScript:

- [Official TypeScript Documentation on Strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [W3Schools Guide on Working with Strings in JavaScript](https://www.w3schools.com/js/js_string_methods.asp)
- [StackOverflow Discussion on Capitalizing Strings in TypeScript](https://stackoverflow.com/questions/37445975/capitalize-first-letter-of-the-string-in-typescript)