---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is a common operation that changes all uppercase characters in a string to lowercase characters. Programmers do this for tasks like normalizing data, comparison, and sorting, as it hampers case sensitivity issues.

## How to:

There is a simple JavaScript method called 'toLowerCase()' which is used for this purpose:

```Javascript
var str = "Hello WORLD";
console.log(str.toLowerCase());
```

This will output:

```Javascript
"hello world"
```

## Deep Dive:

### Historical Context:

Previously in older languages, converting between different cased strings was a manual task involving character code conversions. JavaScript simplifies this significantly with the direct 'toLowerCase()' method.

### Alternatives:

Though 'toLowerCase()' is commonly used, sometimes you need locale-specific conversions. For this, you can use the 'toLocaleLowerCase()' method. For example:

```Javascript
var str = "I love JavaScript";
console.log(str.toLocaleLowerCase('tr-TR')); //Turkish conversion
```
This will output:

```Javascript
"ı love javascript"
```
Notice the 'I' turned into 'ı', not 'i'. This is due to the specificities of the Turkish alphabet.

### Implementational Details: 

The 'toLowerCase()' method doesn't affect special characters or numbers, only alphabetical characters. Also, since strings are immutable in JavaScript, this method doesn't change the original string but returns a new one.

## See Also:

For more advanced string operations, see the following:

- [MDN Web Docs on String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript Kit on Strings and Text](http://www.javascriptkit.com/jsref/string.shtml)
- [W3Schools String Methods tutorial](https://www.w3schools.com/js/js_string_methods.asp)