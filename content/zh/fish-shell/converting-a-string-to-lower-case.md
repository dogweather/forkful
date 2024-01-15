---
title:                "将字符串转换为小写"
html_title:           "Fish Shell: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
有时候，我们需要把字符串转换成小写形式。这可能是因为字符串是由用户输入的，我们希望保持统一的格式，或者是因为跟其他字符串进行比较时需要忽略大小写。

## 如何做到
```Fish Shell```里有内置的函数可以帮助我们把字符串转换成小写形式。让我们来看一个简单的示例：
```
set str "Hello World"
echo $str  # 输出 "Hello World"

# 使用 tolower 函数把字符串转换为小写
set lower_str (tolower $str)
echo $lower_str  # 输出 "hello world"
```
我们可以看到，使用 ```tolower``` 函数可以轻松地把字符串转换成小写形式。另外，如果我们需要在一条命令中同时转换多个字符串，可以使用管道符号 ```|``` 连接多个 ```tolower``` 函数。

## 深入了解
如果你对于字符串的转换操作感兴趣，可以进一步了解内置函数 ```tolower``` 的工作原理。```tolower``` 函数实际上是使用了字符集转换工具 ```iconv``` 来实现字符串的转换。字符集是一种将字符编码为数字的方式，并且不同的字符集可能对应着不同的编码规则。通过转换字符集，字符串中的大写字母会被转换成相应的小写字母。

## 参考链接
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [iconv 文档](http://www.gnu.org/software/libiconv/)
- [字符集和编码简介](https://www.jianshu.com/p/401b6199d1a3)

## 参见