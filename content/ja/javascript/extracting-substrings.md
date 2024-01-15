---
title:                "サブストリングを抽出する"
html_title:           "Javascript: サブストリングを抽出する"
simple_title:         "サブストリングを抽出する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful technique in Javascript for manipulating and extracting specific information from a string. Whether you need to isolate a certain part of a string or perform complex data parsing, substring extraction allows for more efficient and accurate results.

## How To

To extract a substring, you can use the `substring()` method in Javascript. Let's say we have a string called `fruit` that contains the name of a fruit: 

```Javascript
let fruit = "apple";
```

To extract the first three letters of the string, we can use `fruit.substring(0, 3)`, which will return `"app"`. This method takes in two parameters: the start index and the end index (not inclusive). In this example, we specify that we want to start at index 0 and end at index 3, which gives us the first three letters of the string. 

We can also use negative numbers as indices to start counting from the end of the string. So to extract the last three letters of the string, we can use `fruit.substring(-3)`, which will return `"ple"`. This is because `-3` corresponds to the third to last letter in the string.

Substring extraction is not limited to just single characters. You can specify a range of indices to extract multiple characters at once. For example, `fruit.substring(1, 4)` will return `"ppl"`. This is because we start at index 1 (the second letter) and end at index 4 (not inclusive, so the fifth letter). 

## Deep Dive

In addition to the `substring()` method, there are other ways to extract substrings in Javascript. The `substr()` method is similar to `substring()`, but takes different parameters. Instead of specifying the start and end indices, you specify the starting index and the length of the substring you want. For example, `fruit.substr(1, 3)` will return `"pple"`. This is because we start at index 1 and extract a substring with length 3.

Another method is the `slice()` method, which also takes in starting and ending indices as parameters. However, unlike `substring()`, if the start index is greater than the end index, the method will still return the correct substring by switching the values internally. For example, `fruit.slice(3, 1)` will return `"pl"`. This can be useful in certain cases where the starting index is not always smaller than the ending index.

Overall, these substring extraction methods can come in handy for tasks such as manipulating large amounts of data or working with specific portions of strings. Knowing how to properly extract substrings will make your Javascript code more efficient and versatile.

## See Also

Here are some additional resources for learning more about substring extraction in Javascript:

- [MDN Web Docs - substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - substr](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs - slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)