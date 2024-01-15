---
title:                "字符串连接"
html_title:           "Elixir: 字符串连接"
simple_title:         "字符串连接"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
手动连接字符串可能是一项费时费力的任务，特别是当涉及到大量的字符串拼接。使用Elixir内置的字符串连接方法可以让这个任务变得更加有效率和简单。

## 如何操作
通过在字符串之间使用`<>`操作符来连接多个字符串。比如，我们想要将"Hello"和"World"连接起来，可以通过以下方式实现：
```Elixir
"Hello" <> "World"
```
这样，我们就得到了"HelloWorld"作为结果输出。

我们也可以使用多个操作符来同时连接多个字符串。比如，我们想要连接"Hello"、" "、"World"和"!"，可以这样编写代码：
```Elixir
"Hello" <> " " <> "World" <> "!"
```
这样，我们就得到了最终的结果："Hello World!"。

## 深入了解
除了使用`<>`操作符，我们还可以使用`String.concat/2`函数来连接多个字符串。这个函数接受一个包含多个字符串的列表作为参数，并返回一个连接好的字符串。比如，我们想要将"Hello"、" "、"World"和"!"连接起来，可以这样编写代码：
```Elixir
String.concat(["Hello", " ", "World", "!"])
```
这样，我们同样也得到了"Hello World!"作为结果输出。

有时候，我们需要将一个字符串和一个数字连接起来。在这种情况下，我们可以使用`to_string/1`函数将数字转换为字符串，然后再和原来的字符串连接起来。比如，我们想要将"Number"和123连接起来，可以这样编写代码：
```Elixir
"Number" <> to_string(123)
```
这样，我们就得到了"Number123"作为结果输出。

## 参考资料
- [Elixir官方文档 - 字符串拼接](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#string-concatenation)
- [Elixir官方文档 - 字符串操作](https://hexdocs.pm/elixir/String.html)