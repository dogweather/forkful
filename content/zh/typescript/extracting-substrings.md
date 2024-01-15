---
title:                "提取子字符串"
html_title:           "TypeScript: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么
当我们处理字符串时，经常会遇到需要从中提取子字符串的情况。这可能是为了从一个大字符串中提取特定的信息，或者对字符串进行分割和格式化。无论何种情况，提取子字符串是一个非常有用的技巧，可以帮助我们更有效地处理字符串数据。

## 如何操作
提取子字符串的方法在TypeScript中非常简单。我们可以使用`substring()`方法来从给定的字符串中提取一个或多个子字符串，并将其存储在一个新的变量中。下面是一个简单的例子，我们将从一个URL中提取域名部分。

```TypeScript 
const url = "https://www.example.com";
const domain = url.substring(12, 20);

console.log(domain); // 输出: example
```

在上面的代码中，我们使用`substring()`方法来从第12个字符开始提取子字符串，直到第20个字符结束。然后，我们将子字符串存储在`domain`变量中，并通过`console.log()`打印出来。 运行上面的代码，我们将看到子字符串“example”被提取出来并输出到控制台。

除了`substring()`方法，我们还可以使用`substr()`方法来提取子字符串。这个方法接收两个参数，第一个参数是子字符串的开始索引，第二个参数是子字符串的长度。让我们来看一个例子：

```TypeScript 
const text = "Hello World!";
const subText = text.substr(6, 5);

console.log(subText); // 输出: World
```

在上面的代码中，我们使用`substr()`方法从“Hello World!”字符串的第6个字符开始提取长度为5个字符的子字符串。然后我们将该子字符串存储在`subText`变量中，并将其输出到控制台。

## 深入了解
在TypeScript中，我们还可以使用正则表达式来提取子字符串。正则表达式是一种特殊的语法，可以用来匹配和提取特定的文本模式。例如，如果我们想从一个字符串中提取所有的数字字符，我们可以使用正则表达式`/\d+/g`来实现。让我们来看一个例子：

```TypeScript 
const text = "I have 10 apples and 5 oranges.";
const numbers = text.match(/\d+/g);

console.log(numbers); // 输出: [10, 5]
```

在上面的代码中，我们使用`match()`方法来匹配文本中的所有数字（`\d+`），并将它们存储在一个数组中。然后我们将该数组打印到控制台上。

虽然正则表达式可以非常强大，但它也比较复杂。如果你对正则表达式感兴趣，可以参考[正则表达式教程](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions)来深入了解。

## 参考链接
- [TypeScript官方文档](https://www.typescriptlang.org/)
- [使用TypeScript提高JavaScript代码的可靠性](https://www.infoq.cn/article/yzkahztshF9L*JzUACWs)
- [正则表达式教程](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions)