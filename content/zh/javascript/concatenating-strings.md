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

# Why

为什么会使用字符串连接？
字符串连接是在编程中常见的操作，它允许我们将多个字符串拼接在一起形成一个新的字符串。这在构建复杂的文本输出时非常有用，例如打印出用户的信息和动态生成HTML代码。

# How To

如何使用字符串连接？让我们来看一些示例代码和输出结果。

首先，我们定义两个字符串变量，分别包含名字和姓氏。

```Javascript
var firstName = "张";
var lastName = "三";
```

我们可以通过使用 `+` 符号来连接两个字符串，并将结果赋值给一个新的变量。

```Javascript
var fullName = firstName + lastName;
```

输出结果：

```Javascript
张三
```

我们也可以在连接字符串的同时，插入一些其他的文本。

```Javascript
var fullName = firstName + "是这个世界上最棒的" + lastName + "！";
```

输出结果：

```Javascript
张是这个世界上最棒的三！
```

还可以使用 ES6 的字符串模板功能来进行连接，使用反引号 `` ` `` 包裹字符串，并使用 `${}` 来插入变量。

```Javascript
var fullName = `${firstName}家的${lastName}`;
```

输出结果：

```Javascript
张家的三
```

# Deep Dive

深入了解字符串连接。在 Javascript 中，字符串是不可变的，这意味着一旦创建，就无法改变。因此，每次使用 `+` 连接字符串时，实际上都会创建一个新的字符串对象。这可能会在处理大量字符串时产生性能问题。为了避免这种情况，我们可以使用 `+=` 运算符来更高效地连接字符串。

另外，使用字符串模板连接字符串的方式也被认为是更加可读性和简洁的方法。

# See Also

相关链接：
- [MDN 文档 - 字符串连接](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN 文档 - 字符串模板](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Template_literals)