---
title:    "Javascript recipe: Extracting substrings"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful technique in Javascript programming that allows you to isolate and extract specific parts of a string. This can be helpful in situations where you need to manipulate and work with smaller sections of a larger string.

## How To

To extract substrings in Javascript, we can use the `substring()` method. This method takes in two parameters: the starting index and the ending index of the substring you want to extract. For example, if we have the string "Hello World" and we want to extract the substring "World", we would use the following code:

```Javascript
let str = "Hello World";

let substring = str.substring(6, 11);

console.log(substring); // output: World
```

In the above example, the starting index is 6 (corresponding to the first letter of "World") and the ending index is 11 (corresponding to the last letter of "World" plus 1). 

We can also use negative numbers as the parameters for `substring()`, which indicates the starting index from the end of the string. For example, `str.substring(-5, -1)` would extract the substring "Worl" from "Hello World".

It's worth noting that the `substring()` method does not include the character at the ending index in the extracted substring. So in our first example, the character at index 11 (which is the letter "d") is not included in the extracted substring.

## Deep Dive

There are a few other things to keep in mind when using the `substring()` method. If the starting index is greater than the ending index, the parameters will be swapped before the substring is extracted. This means that `str.substring(11, 6)` would still extract the substring "World" from "Hello World".

Additionally, if you omit the second parameter, the `substring()` method will extract the rest of the string starting from the specified index. For example, `str.substring(6)` would extract "World" from "Hello World".

It's also possible to use the `substring()` method in combination with other string methods to extract more complex substrings. For example, we can use the `indexOf()` method to get the starting index of a specific string within a larger string, and then use that index in the `substring()` method to extract a substring. 

```Javascript
let str = "John Doe, age 25";

let name = str.substring(0, str.indexOf(",")); // extracts "John Doe"
let age = str.substring(str.indexOf("age")+4); // extracts "25"

console.log(name); // output: John Doe
console.log(age); // output: 25
```

## See Also

- [MDN documentation for the substring() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String manipulation techniques in Javascript](https://www.w3schools.com/js/js_string_methods.asp)
- [Using the substring() method to extract URLs from a string](https://stackoverflow.com/questions/27663501/extracting-urls-like-http-abc-com-using-javascript-regular-expression)