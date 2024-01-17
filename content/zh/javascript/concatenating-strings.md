---
title:                "连接字符串"
html_title:           "Javascript: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么是字符串连接以及为什么程序员要这么做?

字符串连接是将多个字符串合并成一个新的字符串的过程。程序员通常会使用它来构建动态的消息，如打印日志或生成网页内容。通过连接多个字符串，程序员可以轻松地构造出所需的文字内容。

## 如何实现字符串连接:

### 示例 1: 使用```+```符号

```Javascript
const str1 = "Hello ";
const str2 = "world!";
const newStr = str1 + str2;
console.log(newStr); // Output: Hello world!
```

### 示例 2: 使用```concat()```方法

```Javascript
const str1 = "Hello ";
const str2 = "JavaScript";
const newStr = str1.concat(str2);
console.log(newStr); // Output: Hello JavaScript
```

注意：使用```concat()```方法可以连接多个字符串，例如：```str1.concat(str2, str3)```

## 深入了解:

### 历史背景:

在早期的编程语言中，字符串连接通常需要使用特定的符号来拼接字符串。而在Javascript中，使用```+```符号或```concat()```方法可以更加直观地实现字符串连接，使代码更易读。

### 其他方法:

除了使用```+```和```concat()```外，还可以使用模板字符串```(``` ``` ```)来实现字符串连接，这种方法可以在字符串中直接引用变量或表达式，更加灵活方便。

### 实现细节:

在JavaScript中，字符串连接的本质是将多个字符串按顺序拼接在一起作为一个新的字符串输出。因此，在连接大量字符串时需注意效率问题，避免频繁使用字符串连接操作，而是应该尽可能使用数组和循环来构建新的字符串。

## 链接参考:

- [MDN文档：字符串连接](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN文档：模板字符串](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)