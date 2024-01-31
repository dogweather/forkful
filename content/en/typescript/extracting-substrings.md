---
title:                "Extracting substrings"
date:                  2024-01-20T17:46:57.514519-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means pulling out specific pieces of a string. It's handy for tasks like parsing data, validating input, or just breaking down text into more manageable chunks.

## How to:
In TypeScript, you slice and dice strings with methods like `substring()`, `slice()`, and the ES6 `includes()` for finding text within strings.

```TypeScript
let fullString: string = "Hello, TypeScript enthusiasts!";

// Grab from character 7 to 18
let substr: string = fullString.substring(7, 18);
console.log(substr); // Outputs: TypeScript

// Same deal but with slice()
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // Outputs: TypeScript

// Check if a substring exists
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // Outputs: true
```

## Deep Dive
Once upon a time, string manipulation was more awkward—think C's string functions. Now, JavaScript and TypeScript offer methods that handle Unicode, respect character encoding, and work directly with string objects. `substring()` and `slice()` are similar but with a twist: `slice()` can take negative indices, retroactively counting from the end. `substring()` treats them as zeros. In performance-sensitive situations, picking one over the other can matter, but for day-to-day use, it's pretty much six of one, half a dozen of the other.

```TypeScript
// Using negative index with slice
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // Outputs: Hello, Type
```

As for `includes()`, it's a boon for readability over the classic `indexOf()`, making your intentions clear at a glance. No more `if (string.indexOf('some text') !== -1)`; just a straightforward `if (string.includes('some text'))`.

## See Also
- The TypeScript handbook on strings, for more on how to use `'string'` types: [TypeScript String](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- MDN Web Docs on String methods in JavaScript, applicable for TypeScript: [MDN String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- For understanding more about Unicode and JavaScript (therefore TypeScript), check out [Understanding JavaScript's internal character encoding: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)
