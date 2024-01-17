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

## 什么是HTML解析及其作用?
HTML解析是将HTML代码转换成可读的文本格式的过程。程序员们常常需要进行HTML解析，是因为这样可以轻松地提取特定的文本内容或是数据，并且将其应用于其他的程序中。

## 怎样实现?

使用Fish Shell 进行HTML解析非常简单，在以下的示例中我们将展示如何提取网页中的标题和链接。首先，我们需要安装一个名为“html-xml-utils”的工具，它可以帮助我们解析HTML代码。

```fish
#安装html-xml-utils
brew install html-xml-utils

#提取标题
hxextract -s "<title>" "https://www.example.com"

#提取链接
hxselect -s "a" "https://www.example.com"
```

我们可以在终端中运行以上代码，并且将会得到一个可读的文本输出，其中包括了网页中的标题和链接。

## 深入了解

### 历史背景
HTML解析技术最早出现在上世纪90年代早期，当时，人们开始意识到需要一种方法来将HTML代码转换成可读的文本格式，从而使其更容易被处理和使用。

### 替代方法
除了使用在Shell中使用工具进行HTML解析，也有其他替代方法。例如，可以使用Python中的Beautiful Soup库，或是JavaScript中的Cheerio库来实现同样的功能。

### 实现细节
在Fish Shell中，使用html-xml-utils工具进行HTML解析时，它实际上是调用了一个指令叫“htmltidy”。这个指令是一个开源的工具，通常用于将HTML代码转换成更加干净的格式。通过使用这个工具，我们可以获得更准确和有效的HTML解析结果。

## 查看更多
- [htmltidy GitHub page](https://github.com/htacg/tidy-html5)
- [Beautiful Soup official documentation](https://www.crummy.com/software/BeautifulSoup/)
- [Cheerio official website](https://cheerio.js.org/)