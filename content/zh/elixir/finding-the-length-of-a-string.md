---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

找到字符串的长度是计算和返回字符串中字符的数量。程序员这样做是因为他们需要在数据操作和条件判断等场景中知道字符串的准确长度。

## 如何操作：

这是一个使用Elixir语言计算字符串的长度的例子：
```elixir
string = "你好，世界！"
IO.puts(String.length(string))
```
运行这个例子，返回的结果会是：
```elixir
6
```
这是因为中文中的每一个字符被当作一个单位进行计算的。

## 深度探索

在字符串长度的实现上，Elixir的策略跟其他一些语言就不一样了，因为Elixir中的字符串其实是UTF-8编码的二进制序列，所以可以直接计算出字符串的长度。

另外，Elixir也有其他的方式来得到字符串长度，如使用`byte_size`函数，但这个函数返回的是字符串的字节长度，而不是字符数量。例如：
```elixir
string = "你好，世界！"
IO.puts(byte_size(string))
```
运行这个例子，返回的结果会是：
```elixir
18
```
这是因为每个中文字符使用UTF-8编码需要占用3个字节。

## 另请参阅

- Erlang Org: 关于Elixir的更多信息: [http://www.erlang.org/doc/man/erl.html](http://www.erlang.org/doc/man/erl.html)
- Elixir Official Website: 关于字符串的更多操作: [https://elixir-lang.org/](https://elixir-lang.org/)