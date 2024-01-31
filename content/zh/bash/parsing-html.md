---
title:                "解析HTML"
date:                  2024-01-20T15:30:01.417882-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
解析HTML就是从HTML文档中提取数据的过程。程序员这么做是为了获取网页内容，实现自动化处理。

## How to: (怎么做：)
在Bash中，你可以用各种工具来解析HTML。这儿有一个使用`xmllint`的例子：

```Bash
# 安装xmllint，如果你还没安装的话
sudo apt-get install libxml2-utils

# 利用xmllint解析HTML
xmllint --html --xpath '//h1/text()' example.html
```

这段代码会把`example.html`中所有的`<h1>`标签内容打印出来。

## Deep Dive (深入了解)
最早，我们是手动解析HTML。现在，有很多工具可以做这个，比如`xmllint`、`pup`或`grep`与正则表达式组合。它们各有利弊。比如，`grep`简单快捷但不够精确，`xmllint`符合标准，但使用稍显复杂。选择哪个工具取决于任务复杂度和个人喜好。

## See Also (另请参阅)
- [W3Schools - HTML 教程](https://www.w3schools.com/html/)
- [`xmllint`手册页](http://xmlsoft.org/xmllint.html)
- [Bash 脚本教程](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
