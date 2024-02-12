---
title:                "字符串拼接"
aliases:
- /zh/typescript/concatenating-strings/
date:                  2024-01-20T17:35:49.331863-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串拼接就是将两个或多个字符串合并成一个。程序员这样做是为了创建动态文本、构建URL或者合成用户界面信息。

## How to: (如何操作：)
```TypeScript
// 使用加号拼接字符串
let greeting: string = "你好, ";
let name: string = "小明!";
let welcomeMessage: string = greeting + name;
console.log(welcomeMessage); // 输出: 你好, 小明!

// 使用模板字符串
let welcomeTemplate: string = `${greeting}${name}`;
console.log(welcomeTemplate); // 输出: 你好, 小明!
```

## Deep Dive (深入探索)
在JavaScript的早期版本中，字符串拼接多使用加号(`+`)操作符。但这种方式下如果有很多变量或表达式参与，代码难以阅读和管理。ES6 (ECMAScript 2015)推出了模板字符串(`template strings`)，使用反引号("`")标识，允许嵌入表达式(用`${...}`表示)，使得代码更清晰。

除了清晰度外，模板字符串也处理多行字符串和特殊字符更加方便。而在TypeScript中，拼接字符串的方式与ES6相同，因为TypeScript最终会编译为JavaScript。

在性能方面，现代JavaScript引擎对字符串拼接进行了优化，但大量复杂拼接时，如在循环中，建议使用数组配合`join()`方法。另外，还有第三方库如`lodash`提供了额外的字符串操作功能。

## See Also (另见)
- [MDN Documentation on Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
