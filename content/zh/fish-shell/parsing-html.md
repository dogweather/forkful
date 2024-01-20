---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

解析HTML是一种从HTML文档中提取有用数据的过程。程序员为此需要做很多，因为这是网页爬取，信息提取，甚至某些类型的网络攻击的基础。（Translation: Parsing HTML is the process of extracting useful data from HTML documents. Programmers do it a lot because it's a basis for web scraping, information extraction, and even certain types of cyber attacks.）

## 如何操作：

```Fish Shell 
# 首先，我们安装 html-parser 库
fisher install jorgebucaran/html-parser.fish

# 之后，我们来解析一段简单的HTML。
set html '<html><body><h1>Hello, world!</h1></body></html>'
html-parse $html -n
```
过程操作后将会显示：
```Fish Shell
document: 1
  html: 2
    body: 3
      h1: 4
        #text: Hello, world!
```

## 深入研究

HTML解析的历史可以追溯到HTML的诞生。随着HTML的发展和更多复杂网站的出现，提取HTML信息变得越来越重要。(Translation: The history of HTML parsing traces back to the birth of HTML. As HTML evolved and more complex websites were born, extracting information from HTML became more and more important.)

Fish Shell并非解析HTML的唯一工具。有许多其他解析器，如Python的BeautifulSoup，或Node.js的cheerio等等。每个工具有自己的优缺点，你可以根据需求选择适合的工具。(Translation: Fish Shell is not the only tool to parse HTML. There are many other parsers such as BeautifulSoup in Python, cheerio in Node.js, and so on. Each tool has its strengths and weaknesses and you can choose the right tool based on your needs.)

在Fish Shell中，我们使用html-parser库，它是用Fish Script编写的。它简单，高效，并且易于使用，适合需要浅层数据提取的场景。(Translation: In Fish Shell, we use the html-parser library, which is written in Fish Script. It's simple, efficient, and easy to use, suitable for scenarios requiring shallow data extraction.)

## 另请参阅

- [Fish Shell官方网站](https://fishshell.com): 更多关于Fish Shell的信息和示例。
- [html-parser库](https://github.com/jorgebucaran/html-parser.fish): html-parser库的具体使用和实现细节。
- [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): 强大的Python库，可进行深度HTML解析。(Translation: A powerful Python library for deep HTML parsing)