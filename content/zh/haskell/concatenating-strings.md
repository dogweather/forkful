---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 是什么？为什么？
字符串拼接是将两个或更多的字符串联、连在一起以形成一个新的字符串。程序员之所以这样做，主要是为了创建更复杂的文本结构，或者通过拼接构建新的数据。

## 如何操作：
Haskell中，更倾向于使用'++'运算符进行字符串拼接。以下是示例及其输出：

```Haskell
main = do
    let str1 = "Hello"
    let str2 = "World"
    putStrLn (str1 ++ " " ++ str2)
```
输出：

```Haskell
Hello World
```

此段代码将字符串"Hello"和"World"拼接起来，并在两者之间添加了一个空格。

## 深入探索
字符串拼接在计算机科学历史上一直是常见操作。在Haskell中，字符串实质上是字符列表。字符串拼接就相当于合并两个列表。因此，任何关于列表联接的操作都可以应用于字符串联接。

在拼接字符串时，还有其他的一些选择，例如使用`concat`函数拼接一个字符串列表等。

```Haskell
main = do
    let words = ["Hello", " ", "World"]
    putStrLn (concat words)
```
输出：
```Haskell
Hello World
```

`concat`函数从Haskell的标准库Data.List中导入。这对于拼接大量的字符串尤其有用，因为它可以避免`++`运算符所带来的额外开销。

## 更多参考
查看其他相关文档，来了解更多关于Haskell中字符串相关：
- Haskell 字符串操作: https://wiki.haskell.org/String
- Haskell初学者教程：https://www.runoob.com/haskell/haskell-tutorial.html