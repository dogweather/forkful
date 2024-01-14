---
title:    "TypeScript recipe: Using regular expressions"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why
Regular expressions are an essential tool for any programmer, as they allow for powerful and efficient string manipulation. By using regular expressions, you can search, extract, and replace specific patterns within a string, making it a must-have skill for any developer.

## How To
To use regular expressions in TypeScript, follow these simple steps:

1. First, import the `RegExp` class from the `@types/node` library:

```TypeScript
import { RegExp } from '@types/node';
```

2. Create a regular expression by using the `RegExp` constructor and passing in a string with the desired pattern, along with any flags:

```TypeScript
const regex = new RegExp('foo', 'g');
```

3. Use the `test()` method to check if a string matches the regular expression:

```TypeScript
regex.test('foobar'); // true
regex.test('bar'); // false
```

4. Use the `match()` method to extract the matched pattern from a string:

```TypeScript
'foobar'.match(regex); // ['foo']
```

5. Use the `replace()` method to replace the matched pattern with a new string:

```TypeScript
'hello foo'.replace(regex, 'world'); // 'hello world'
```

## Deep Dive
Regular expressions can be used for more complex string manipulation. They can include special characters and quantifiers to match certain patterns, as well as capture groups to extract specific parts of a string. For example:

- The `.` character matches any single character. So the regular expression `f.o` would match 'foo', 'fro', and 'f3o'.
- The `+` quantifier matches one or more of the previous character. So the regular expression `fo+` would match 'foo' and 'foooo', but not 'f'.
- Capture groups can be used to extract specific parts of a string. For example, the regular expression `(foo)bar` would match 'foobar' and capture 'foo' in a group, which can be accessed through the `match()` method.

Overall, regular expressions may seem daunting at first, but with practice, they can become a powerful and valuable tool in your programming toolbox.

## See Also
For more information on regular expressions and their usage in TypeScript, check out these helpful resources:

- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation on Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regexr: Interactive Regular Expression Testing](https://regexr.com/)