---
title:                "字符串首字母大写"
date:                  2024-01-19
simple_title:         "字符串首字母大写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
在JavaScript中，大写化字符串意味着将文本中的所有字母统一转换为大写形式。程序员通常这样做来规范数据输入，或者在用户界面中为了一致性或强调某些文字。

## How to: (怎么做：)
JavaScript提供了一个简单的方法来大写字符串。用`.toUpperCase()`这个方法就可以了，示例代码如下：

```Javascript
let greeting = '你好，世界！';
let shout = greeting.toUpperCase();

console.log(shout);  // 输出：你好，世界！
```

如果要每个单词的首字母大写，可以这样做：

```Javascript
let title = 'javascript编程简介';
let capitalizedTitle = title.split(' ')
                             .map(word => word[0].toUpperCase() + word.substring(1))
                             .join(' ');

console.log(capitalizedTitle);  // 输出：Javascript编程简介
```

## Deep Dive (深度解析)
在早期的编程语言中，大写化字符串通常需要通过遍历每个字符并手动转换来实现。现在，JavaScript 的 `.toUpperCase()` 方法为我们提供了一种快速、简便的大写化方式。但要注意的是，它对于某些语言中的特殊字符可能不会按预期工作。

若不使用 JavaScript 自带的方法，你也可以自己实现一个大写化函数。譬如，你可以利用 ASCII 值来转换英文字符。

除了 `.toUpperCase()`，还有 `.toLowerCase()` 转为小写，`.toLocaleUpperCase()` 和 `.toLocaleLowerCase()` 分别根据本地语言环境转换大小写，更适合某些特定语言。

在处理用户输入或显示数据时，大小写通常需要根据上下文适当转换。大写化通常用于表头、标题或用户界面的按钮上的文本。

## See Also (参考链接)
- JavaScript String reference on MDN: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- String methods in JavaScript: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- ASCII table and description: [https://www.asciitable.com/](https://www.asciitable.com/)
