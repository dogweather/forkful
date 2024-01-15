---
title:                "将字符串大写"
html_title:           "Python: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

字符串大写首字母是一个常见的操作，它可以让文本更加美观，也有助于标识各个单词的开头。在Python中，有一个内置的函数可以轻松实现这一功能。

## 如何

首先，我们需要一个字符串变量，这可以是用户输入的文本或者是我们自己指定的。接下来，我们可以使用`capitalize()`函数来将字符串的首字母大写。下面是一个简单的例子：

```Python
text = "hello world"
capitalized_text = text.capitalize()
print(capitalized_text)
```

运行这段代码，我们会得到输出：`Hello world`。可以看到，字符串`hello`的首字母H变成了大写。同时，在字符串中间的空格和其他字符不会受到影响。

另外，如果字符串的首字母已经是大写，那么`capitalize()`函数不会做任何改变。例如，字符串`World`仍然会被返回为`World`。

## 深入了解

除了`capitalize()`函数外，Python中还有几种方法可以实现字符串大写首字母。其中，`title()`函数可以将每个单词的首字母都大写，而不仅仅是第一个字母。另外，`upper()`函数可以将整个字符串的字母都变成大写。

需要注意的是，字符串的大小写改变是一种不可逆的操作。意思就是说，一旦改变了字符串的大小写，就无法再恢复成原来的样子。因此，在使用这些函数时，要小心选择。

## 参考资料

- [Python官方文档：String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [字符串大小写转换的几种方法](https://blog.csdn.net/isea533/article/details/79989993)

## 参见

- [Python中内置函数`capitalize()`的用法](https://www.runoob.com/python3/python3-string-capitalize.html)
- [如何在Python中检查字符串是否以大写字母开头](https://www.w3schools.com/python/ref_string_istitle.asp)