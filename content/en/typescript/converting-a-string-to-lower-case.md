---
title:    "TypeScript recipe: Converting a string to lower case"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to compare two strings but they were not in the same case? This can lead to unexpected results and can be frustrating to deal with. Converting strings to lower case is a common solution to this problem, and in this blog post, we will explore why it's important to do so and how to achieve it using TypeScript.

## How To

Converting a string to lower case in TypeScript is a simple task. You can use the built-in `toLowerCase()` method which is available on all string objects.

```TypeScript
let string = "Hello, World!";
let lowerCaseString = string.toLowerCase();

console.log(lowerCaseString); // output: hello, world!
```

In the above example, we first declare a string variable with the value "Hello, World!" and then use the `toLowerCase()` method to convert it to lower case. We then log the result to the console and see the expected output.

But what if you have a string that contains special characters or accented letters? For these cases, you can use the `toLocaleLowerCase()` method, which uses the current locale's rules for lowercasing characters.

```TypeScript
let string = "MÁGICáL";
let lowerCaseString = string.toLocaleLowerCase();

console.log(lowerCaseString); // output: mágicál
```

As you can see, the special characters and accented letters are converted to their respective lower case versions based on the current locale. This is useful for handling strings in different languages.

## Deep Dive

Behind the scenes, the `toLowerCase()` and `toLocaleLowerCase()` methods are executed using the Unicode standard. This means that the conversion is not limited to just the English alphabet, but also includes other languages and special characters.

The Unicode standard defines a set of rules for lowercasing characters, which takes into account different writing systems, such as Latin, Greek, Cyrillic, and more. This ensures that the methods work accurately for a wide range of languages.

Furthermore, the `toLocaleLowerCase()` method also takes into account the locale's language-specific rules for case conversion. For example, in the Turkish language, the uppercase "I" is converted to the lowercase "ı", whereas in English, it is converted to "i". This distinction is important to ensure proper string comparison in different languages.

## See Also

- [TypeScript String Methods](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#in-typescript)
- [Unicode Lowercase Mapping](https://unicode.org/reports/tr21/tr21-5.html#Mapping_Table)
- [Lowercase Conversions in Different Languages](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase#conversion in different languages)

By converting strings to lower case, you can avoid errors and ensure accurate string comparisons in various languages. So next time you encounter a case sensitivity issue with strings, remember to use the `toLowerCase()` or `toLocaleLowerCase()` methods in your TypeScript code. Happy coding!