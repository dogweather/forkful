---
title:                "Javascript: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么
在Javascript编程中，我们经常会遇到需要将字符串（文字）的首字母大写的情况。比如说在一个程序中需要输出一个人的姓名，但是我们希望姓名的格式是首字母大写的。这时候就需要用到字符串首字母大写的方法。

## 如何
在Javascript中实现字符串首字母大写非常简单。我们可以使用内置函数`toUpperCase()`来实现，该函数会将字符串中的所有字符都转换为大写字母。但是这样做会将字符串中的所有字符都转换为大写，而不仅仅是首字母。所以我们需要多做一些处理来仅限制首字母大写。

首先，我们需要使用`charAt()`函数来获取字符串中的第一个字符，并将其转换为大写字母。然后，我们可以使用`slice()`函数来获取从第二个字符开始到字符串末尾的所有字符，并用`toUpperCase()`将它们转换为大写字母。最后，我们使用`concat()`函数将转换后的首字母和剩余部分重新拼接成一个新的字符串。下面是一个代码示例：

```Javascript
var string = "javascript";
var firstChar = string.charAt(0).toUpperCase();
var restOfString = string.slice(1).toUpperCase();
var capitalizedString = firstChar.concat(restOfString);
console.log(capitalizedString);
```

输出结果为：

```
Javascript
```

## 深入探讨
除了上面提到的方法，还有其他多种实现字符串首字母大写的方式。其中一种方法是使用正则表达式来匹配首字母，然后将其替换为大写字母。这种方法的好处是可以一次性处理多个字符串。另外还可以使用第三方库来实现字符串首字母大写，如`lodash`库中的`capitalize()`函数。

实现字符串首字母大写的方法有很多种，选择哪种方法取决于具体的需求和偏好。在编程中，我们应该学会多样化的解决问题的方式，并选择最适合自己的方法。

## 参考资料
1. [JavaScript字符串的首字母大写技巧](https://blog.csdn.net/myarrow/article/details/53104564)
2. [开发示例：字符串首字母大写转换](https://www.cnblogs.com/xuanyuan/p/6794956.html)
3. [JavaScript中字符串首字母大写的几种方法](https://www.jb51.net/article/154431.htm)

## 参见