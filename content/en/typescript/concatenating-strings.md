---
title:                "TypeScript recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

In programming, it is often necessary to combine multiple strings into one. This process is known as string concatenation and can be useful for tasks such as displaying text or creating file names. In this blog post, we will explore how to perform string concatenation in TypeScript.

## How To

To begin, let's first understand how string concatenation works. In TypeScript, there are two ways to concatenate strings: using the `+` operator or the `concat()` method. Let's take a look at some examples below:

```TypeScript
// Using + operator
let str1 = "Hello";
let str2 = "world";
let result = str1 + " " + str2;
console.log(result); // Output: Hello world

// Using concat() method
let str3 = "Good";
let str4 = "morning";
let result2 = str3.concat(" ", str4);
console.log(result2); // Output: Good morning
```

As you can see, both the `+` operator and `concat()` method allow us to combine multiple strings together. However, it is important to note that the `+` operator has a slight edge in terms of performance as it directly adds the strings together, while the `concat()` method creates a new string from the existing ones.

Additionally, we can also perform string concatenation with template literals. Template literals are enclosed by backticks and allow us to interpolate variables within the string. Let's see an example:

```TypeScript
let name = "John";
let greeting = `Hello ${name}, welcome to my blog!`;
console.log(greeting); // Output: Hello John, welcome to my blog!
```

It is also worth mentioning that we can concatenate strings in a loop by using the `+=` or `concat()` method. This can be useful in cases where we need to build a larger string from smaller chunks.

## Deep Dive

Under the hood, string concatenation in TypeScript is actually handled by the `String` class. The `+` operator and `concat()` method both leverage the `String.fromCharCode()` function to combine the strings.

It is also important to note that string concatenation can be affected by type coercion. This means that if we try to concatenate a string with another data type, TypeScript will try to convert the other value into a string before performing the concatenation. Let's see an example:

```TypeScript
let num = 123;
let result = "The number is " + num;
console.log(result); // Output: The number is 123
```

In the example above, the number `123` was converted to a string before being concatenated with the other string. This is where template literals come in handy as they automatically convert any interpolated variables into strings without the need for type coercion.

## See Also

If you want to learn more about TypeScript string concatenation, check out these helpful resources:

- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Concatenating strings in TypeScript](https://medium.com/@nilobarp/concatenating-strings-in-typescript-1ca11821779e)
- [TypeScript String Operators](https://www.tutorialspoint.com/typescript/typescript_string_operators.htm)