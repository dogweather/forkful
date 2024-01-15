---
title:                "解析HTML"
html_title:           "Fish Shell: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

对于程序员来说，解析 HTML 是一个常见的任务。它可以让我们从网页中提取出有用的信息，帮助我们用更有效的方式处理数据。Fish Shell 正是为了方便我们处理这种任务而设计的。

## 如何使用

想要在 Fish Shell 中解析 HTML，你需要安装 `curl` 和 `pup` 工具。 `pup` 是一个用于解析 HTML 的命令行工具，它支持 CSS 选择器和 XPath 表达式。下面是一个基本示例：

```fish
curl -s https://www.example.com | pup 'h1 text{}'
```

这个命令会从指定网页中提取标题，并输出到终端。如果你想要提取多个元素，可以使用 `-f` 指定输出格式：

```fish
curl -s https://www.example.com |
    pup '.card text{}' |
    awk 'BEGIN{RS=""}{$1=$1}1' OFS=\n > output.txt
```

这个命令会提取网页中所有带有 `.card` 类的元素，并将每个元素的内容以换行符分隔保存到 `output.txt` 文件中。

## 深入了解

除了基本的 CSS 选择器和 XPath 表达式，`pup` 还支持一些高级功能，如条件选择和输出格式。你可以通过 `pup --help` 命令查看所有的选项和使用说明。

另外，`pup` 还支持使用 JavaScript 对 HTML 文档进行修改和操作，这为解析复杂的页面提供了更多的灵活性。你可以通过在 `pup` 命令后加上 `-f` 参数来指定一个内联的 JavaScript 函数来实现这一功能。

## 参考资料

- [pup 官方文档](https://github.com/ericchiang/pup)
- [示例代码](https://github.com/ericchiang/pup#bash-example)
- [XPath 表达式简易教程](https://www.w3school.com.cn/xpath/xpath_syntax.asp)

## 参见

- [CSS 选择器简易教程](https://www.runoob.com/cssref/css-selectors.html)
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)