---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

字符串提取是从一个已存在的字符串获取一部分字符串 (成为子串) 的过程。它在处理文本数据和编程时很有用,比如分析用户输入或是切分数据。

## 如何操作：

在 TypeScript 里，让我们来看看如何或是从哪里提取子串。首先，我们创建一个 'sourceString'：

```TypeScript
let sourceString: string = "Hello, TypeScript.";
```
接下来，我们使用 'substring' 函数从 'sourceString' 提取子串：

```TypeScript
let subString: string = sourceString.substring(7, 17);
console.log(subString); // 输出 "TypeScript"
```
你也可以使用 'slice' 函数来提取子串，如下所示：

```TypeScript
let subString: string = sourceString.slice(7, 17);
console.log(subString); // 输出 "TypeScript"
```
两种方法都能得到同样的结果。

## 深入了解

使用上述方法在历史上有一些争议，因为 'substring' 和 'slice' 函数在处理负参数时的行为不同。这点在不同编程语言和版本中可能会有差异。另外，另一种方法 'substr()' 已被弃用，不推荐在 TypeScript 中使用。

值得注意的是，这些字符串提取函数速度很快，因为它们并不更改源字符串，而是创建一个新的字符串。

如果你需要提取字符串，在 TypeScript 中使用 'substring' 或 'slice' 是最好的选择。

## 参考资料

* MDN Documentation on substring: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
* MDN Documentation on slice: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
* TypeScript Official Website: [http://www.typescriptlang.org/](http://www.typescriptlang.org/)