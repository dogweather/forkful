---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
字符串连接就是将两个或多个字符串以特定顺序连接到一起。程序员使用这种技术以便节省内存，以及增强代码的可读性和可维护性。

## 如何操作：
在TypeScript中，可以用加号 (+) 或反撇号 (`) 来进行字符串连接。看下面的例子：

```TypeScript
let string1 = 'Hello, ';
let string2 = 'World!';
let result = string1 + string2;
console.log(result); // 输出: 'Hello, World!'
```

我们也可以用反撇号 (`) 创造模板字符串来进行字符串的连接，像这样：

```TypeScript
let string1 = 'Hello, ';
let string2 = 'World!';
let result = `${string1}${string2}`;
console.log(result); // 输出: 'Hello, World!'
```

## 深入探讨
在早期的编程语言中，例如C语言和Pascal，程序员需要花费大量的时间和精力将字符串连接在一起。而现在，现代编程语言, 例如TypeScript, 提供了更加直观和简单的方法进行字符串连接。

除了加号 (`+`) 和反撇号 (`)，我们也可以使用 `concat()` 方法实现字符串的连接。但需要注意的是，`concat()` 方法在多个字符串连接时性能相对较差。

时刻记住，虽然我们有多种方法来实现字符串连接，但关键是选择最适合你项目需求的方式。

```TypeScript
let string1 = 'Hello, ';
let string2 = 'World!';
let result = string1.concat(string2);
console.log(result); // 输出: 'Hello, World!'
```

## 扩展阅读
1. [TypeScript官方文档](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html): 提供了有关字符串模板的详细信息。
2. [MDN Web文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/concat): 对字符串的 `concat()`方法的详细说明。

没有“总结”部分。