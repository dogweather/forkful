---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

解析HTML就是从HTML文档内容中提取具体的信息。程序员做这个主要是为了获取网页中的数据，或者进行网页内容的操作和修改。

## 如何操作:

让我们通过用Elixir编写一段简单的代码来解析HTML。我们将使用Floki库。

首先，安装Floki库:
```elixir
  defp deps do
    [
      {:floki, "~> 0.30.1"}
    ]
  end
```
然后，获取HTML信息并解析:
```elixir
  html = "<div><a href='http://elixir-lang.github.io/'>Elixir</a></div>"
  {:ok, parsed_html} = Floki.parse_document(html)
  links = Floki.find(parsed_html, "a")
```
输出如下
```elixir
[
  {"a", [{"href", "http://elixir-lang.github.io/"}], ["Elixir"]}
]
```

## 深入探究

解析HTML的发展早在网页出现之初就已经开始了。人们开始认识到在处理大量网页信息时，自动化提取信息的技术的价值，这就是解析HTML的来源。

解析HTML的代替方案包括使用正则表达式，并进行手动解析。然而这些通常比使用成熟的库更困难，且容易出错。

在实现方面，解析HTML涉及到计算机科学中的许多基本问题，包括数据结构（例如树）和算法。库中的解析引擎首先将HTML分割成元素和属性，然后将它们组合在一起形成DOM（文档对象模型）树。

## 参考链接

Elixir的Floki库: https://github.com/philss/floki

HTML的DOM操作教程: https://www.w3schools.com/js/js_htmldom.asp

HTML解析的基础知识: https://htmlparser.info/parser/