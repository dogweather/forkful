---
title:                "Javascript recipe: Capitalizing a string"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with strings in Javascript, it is often necessary to format them in a specific way. One common formatting task is to capitalize the first letter of each word in a string. This is especially useful when dealing with names or titles, but it can also be used to improve the overall readability of a string. In this blog post, we will explore how to capitalize a string in Javascript and why it may be useful to do so.

## How To

In order to capitalize a string in Javascript, we can use the built-in `toUpperCase()` function along with some string manipulation techniques. Here is an example code block showing how we can capitalize a string:

```Javascript
let str = "hello world";
let capitalizedStr = str.split(" ")   // split the string into an array of words
                       .map(word => word.charAt(0).toUpperCase() + word.slice(1))   // capitalize first letter of each word
                       .join(" ");   // join the words back into a string
console.log(capitalizedStr);   // output: "Hello World"
```

In the code, we first use the `split()` function to split the original string into an array of individual words. Then, using the `map()` function, we apply a function to each word which capitalizes its first letter and then adds the rest of the word. Finally, we use the `join()` function to join the capitalized words back into a single string. The result is a new string with the first letter of each word capitalized. 

## Deep Dive

There are a few things to note in the above code block. The first is the use of the `split()` function with a space as the delimiter. This allows us to split the string into individual words based on the spaces between them. Next, the `map()` function is used to apply the same operation to each element in the array, in this case, capitalizing the first letter of each word.

Secondly, we use the `charAt()` function to select the first letter of each word. This function takes in an index and returns the character at that position. In this case, we use the index `0` to select the first letter of each word. Finally, we use the `slice()` function to take a portion of the word starting from index `1`, which is the second letter. This results in a capitalized first letter followed by the rest of the word.

## See Also

- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Array.prototype.map()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [String.prototype.split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)