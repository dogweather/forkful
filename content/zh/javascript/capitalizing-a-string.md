---
title:                "将字符串转换为大写"
html_title:           "Javascript: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

首先，让我们明确一下什么是字符串的大写。在Javascript中，字符串指的是一系列的字符，可以是字母、数字、符号等。将字符串变成大写意味着将所有的字符转换成大写字母。

那么为什么会需要对字符串进行大写转换呢？最常见的情况是为了统一格式，例如在用户输入时，要求用户名必须以大写字母开头，那么我们就需要将用户输入的字符串转换成大写。另外，对于搜索功能来说，忽略大小写可以提高搜索结果的准确性，这也是通过将字符串统一转换成大写来实现的。

## 如何做

在Javascript中，我们可以使用`.toUpperCase()`方法来将字符串转换成大写形式。例如，我们有一个变量`str`的值为"hello world"，那么我们可以通过以下代码将它转换成大写形式并输出到控制台：

```Javascript
console.log(str.toUpperCase());
// Output: HELLO WORLD
```

除了单纯的输出，我们也可以将大写形式的字符串赋值给一个新的变量，例如：

```Javascript
var newStr = str.toUpperCase();
console.log(newStr);
// Output: HELLO WORLD
```

## 深入探讨

除了使用现成的方法，我们也可以通过编写自定义函数来实现字符串的大写转换。以下是一个简单的示例：

```Javascript
function toUpperCase(str) {
  var result = "";
  for (var i = 0; i < str.length; i++) {
    var char = str.charAt(i);
    // 检查字符是否为小写字母，若是，则通过字符编码转换为大写
    if (char.charCodeAt() >= 97 && char.charCodeAt() <= 122) {
      result += String.fromCharCode(char.charCodeAt() - 32);
    } else {
      result += char;
    }
  }
  return result;
}

console.log(toUpperCase(str));
// Output: HELLO WORLD
```

通过自定义函数，我们可以更加深入地了解字符串的组成结构和字符编码，同时也可以根据实际需求添加更多的功能。

## 参考链接
- [The `toUpperCase()` method - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String data type - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Character Encodings - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Text_formatting#Character_Encodings_Overview)