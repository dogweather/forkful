---
title:                "Concatenating strings"
html_title:           "TypeScript recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

When working with strings in TypeScript, there may be situations where you need to combine multiple strings into one. This process is known as concatenation and is a common operation in programming. By understanding how to concatenate strings in TypeScript, you can easily manipulate and manipulate text data in your code.

## How To

To concatenate strings in TypeScript, you can use the '+' operator or the 'concat' method. Let's look at an example using the '+' operator:

```TypeScript
let greeting: string = "Hello";
let name: string = "John";
let message: string = greeting + " " + name;
console.log(message); // Outputs "Hello John"
```

In the above example, we have three strings: greeting, name, and message. By using the '+' operator, we can combine the greeting and name strings to create a new string, assigned to the message variable. This newly concatenated string is then displayed in the console.

You can also use the 'concat' method, which works similarly to the '+' operator:

```TypeScript
let message: string = greeting.concat(" ", name);
console.log(message); // Outputs "Hello John"
```

It is important to note that both the '+' operator and the 'concat' method do not modify the original strings. Instead, they return a new string which is the result of the concatenation.

## Deep Dive

In TypeScript, strings are immutable, meaning they cannot be changed once they are created. This is why the concatenation operation returns a new string instead of modifying the existing ones.

There are a few things to keep in mind when concatenating strings in TypeScript:

- You can concatenate any number of strings together, as shown in the examples above.
- You can also concatenate strings with other types, such as numbers, by converting them to strings first.
- The order in which you concatenate strings matters, as it affects the final result. For example, "2" + "2" will result in "22", while "2" + 2 will result in "22".

Overall, concatenating strings in TypeScript is a simple and useful operation that allows you to manipulate text data in your code.

## See Also

If you want to learn more about working with strings in TypeScript, here are some helpful resources:

- [TypeScript Strings Documentation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Concatenating Strings in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript Official Website](https://www.typescriptlang.org)

Happy coding!