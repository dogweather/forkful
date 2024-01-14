---
title:    "Javascript: 提取子字符串"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串

提取子字符串是一种常见的编程技术，它可以帮助我们从一个大的字符串中提取出需要的部分。这在处理文本和数据时非常有用，让我们更灵活地操作字符串。

# 如何提取子字符串

提取子字符串最简单的方法是使用JavaScript中的 `substring()` 函数。我们需要传入两个参数，第一个参数是起始位置的索引，第二个参数是终止位置的索引（可选）。

```Javascript
let str = "Hello World!";
let substring = str.substring(0, 5);
console.log(substring); // Output: Hello
```

我们还可以使用 `slice()` 函数来提取子字符串，它的工作方式与 `substring()` 类似，但是它也可以接受负数作为参数来表示相对于字符串末尾的位置。

```Javascript
let str = "Hello World!";
let substring = str.slice(6, -1);
console.log(substring); // Output: Worl
```

如果我们想要提取出字符串中的最后几个字符，我们可以使用 `substr()` 函数，它接受两个参数，第一个参数是起始位置的索引，第二个参数是要提取的字符数量。

```Javascript
let str = "Hello World!";
let substring = str.substr(6, 3);
console.log(substring); // Output: Wor
```

# 深入了解提取子字符串

除了上面提到的三个函数之外，还有一些其他的JavaScript字符串函数可以用来提取子字符串，如 `split()` 函数可以根据指定的分隔符将字符串分割成一个数组，我们可以根据数组的索引来获取需要的子字符串。此外，还有一些正则表达式的方法也可以用来提取子字符串，这些方法将在后续的文章中进行介绍。

# 参考链接

- [MDN - substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - substr()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN - split()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/split)