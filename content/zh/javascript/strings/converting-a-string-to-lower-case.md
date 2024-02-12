---
title:                "将字符串转换为小写"
aliases:
- /zh/javascript/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:44.300450-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
将字符串转换为小写主要是为了统一数据格式。程序员这么做是为了能够在比较或处理文本时忽略大小写的差异。

## How to: (如何操作：)
JavaScript 提供了一个简单的方法 `.toLowerCase()` 来把字符串转换成小写字母。

```javascript
let greeting = "Hello, World!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // 输出: "hello, world!"
```

## Deep Dive (深入了解)
在 JavaScript 早期版本中就已经引入了 `.toLowerCase()`，这个方法根据基本的 Unicode 标准将字符串中的所有大写字母转换为相应的小写字母。

替代方案包括使用正则表达式和 `.replace()` 方法，但这通常不必要，因为 `.toLowerCase()` 已经足够高效且易于使用。在实现细节方面，`.toLowerCase()` 会考虑到地区特定的字符映射，这意味着在不同语言环境下，它能正确处理特定字符的大小写转换。

```javascript
let turkishGreeting = "Merhaba DÜNYA";
let lowerCaseTurkishGreeting = turkishGreeting.toLowerCase();
console.log(lowerCaseTurkishGreeting); // 输出: "merhaba dünya"（考虑了土耳其语中的特殊字符）
```

## See Also (另请参见)
- MDN web docs 上的 `.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Unicode 案例映射: https://www.unicode.org/reports/tr21/tr21-5.html
- JavaScript 正则表达式: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
