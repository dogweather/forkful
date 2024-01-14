---
title:    "TypeScript: 提取子字符串"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

通过提取子字符串可以方便地从一个字符串中获取需要的部分，例如从一个完整的字符串中提取出特定的单词或者字符。这在处理文本数据时非常有用。

## 如何操作

要提取子字符串，我们可以使用`.slice()`方法，它接受两个参数，第一个参数是开始位置，第二个参数是结束位置（不包括在内）。我们也可以使用`.substring()`方法，它接受两个参数，第一个参数是开始位置，第二个参数是结束位置（包括在内）。让我们来看一下下面的代码示例：

```TypeScript
// 定义一个字符串
let str = "Hello, world!";
// 使用slice提取"world!"部分
let extracted1 = str.slice(7, 13);
console.log(extracted1); // 输出 "world!"
// 使用substring提取"world!"部分
let extracted2 = str.substring(7, 13);
console.log(extracted2); // 输出 "world!"
```

在上面的代码中，我们使用`.slice()`方法和`.substring()`方法提取了同样的子字符串。需要注意的是，`.slice()`方法和`.substring()`方法都可以接受负数作为参数，代表从字符串的末尾计算位置。例如，如果我们将`-1`作为结束位置，就可以提取出字符串的最后一个字符。让我们来看一下下面的代码示例：

```TypeScript
// 定义一个字符串
let str = "Hello, world!";
// 使用slice提取最后一个字符
let extracted1 = str.slice(12);
console.log(extracted1); // 输出 "!"
// 使用substring提取最后一个字符
let extracted2 = str.substring(-1);
console.log(extracted2); // 输出 "!"
```

## 深入了解

在提取子字符串时，我们还可以使用`.substr()`方法，它接受两个参数，第一个参数是开始位置，第二个参数是从开始位置起要提取的字符数量。除此之外，在提取子字符串时，我们还可以使用正则表达式，结合`.match()`方法或`.replace()`方法提取出符合某种规则的子字符串。

## 另请参阅

- [TypeScript官方文档：String API](https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN Web文档：substring()方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web文档：match()方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/match)
- [MDN Web文档：replace()方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/replace)