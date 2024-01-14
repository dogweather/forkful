---
title:                "TypeScript: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写

在开发软件时，有时候我们需要比较两个字符串是否相同。然而，字符串之间的比较是区分大小写的，意思就是说一个大写的字母和一个小写的字母被认为是不同的。为了避免这种情况，我们常常会将字符串转换为小写，这样就可以确保比较的准确性。

## 如何做

在TypeScript中，将字符串转换为小写可以通过使用toLowerCase()方法来实现。下面是一个例子：

```TypeScript
let str = "Hello World";
console.log(str.toLowerCase()); // 输出 "hello world"
```
通过调用toLowerCase()方法，我们可以将字符串"Hello World"转换为全小写的"hello world"。

## 深入探讨

除了使用toLowerCase()方法，还有其他几种方法可以将字符串转换为小写。我们可以使用正则表达式来匹配所有大写字母，然后将它们替换为对应的小写字母。还可以使用循环和条件语句来逐个检查字符串中的每个字符，并将大写字母转换为小写字母。无论使用哪种方法，重点都是要确保字符串中的所有字符都是小写的。

# 参考链接

- https://www.typescriptlang.org/docs/handbook/functions.html#the-this-parameter
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- https://www.w3schools.com/jsref/jsref_touppercase.asp

# 参见

其他有用的字符串操作方法：

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace