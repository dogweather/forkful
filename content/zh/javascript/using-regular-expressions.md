---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
正則表達式，就是匹配字符串的模式。程式設計師用它來搜尋、替換和檢驗文本，節省時間且提升效率。

## How to:
```javascript
// 搜索特定字詞
let text = "學習JavaScript很有趣。";
let pattern = /有趣/;
console.log(pattern.test(text)); // 輸出: true

// 替換字串
text = text.replace(/JavaScript/, "JS");
console.log(text); // 輸出: "學習JS很有趣。"

// 校驗格式：檢查電郵
let email = "example@example.com";
let emailPattern = /^\w+@\w+\.\w+$/;
console.log(emailPattern.test(email)); // 輸出: true
```

## Deep Dive
正則表達式起源於1950年代，由數學家Stephen Kleene提出。除了JavaScript, 其他語言如Python, Ruby, Java都支援正則表達式。實作細節方面，JavaScript的正則表達式遵守ECMAScript標准。值得注意的另類選擇包括字符串的內建方法，如`startsWith()`, `endsWith()`, 和`includes()`，但它們的功能更簡單、專一。

## See Also
- MDN Web Docs上正則表達式教學：[這裡](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101，一個測試正則表達式的線上工具：[這裡](https://regex101.com/)
