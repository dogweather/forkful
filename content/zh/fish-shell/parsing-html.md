---
title:                "解析HTML"
date:                  2024-01-20T15:31:43.790663-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "Fish Shell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
解析HTML是指提取网页代码中的数据及结构信息。程序员这样做是为了自动化处理网页内容，比如数据抓取或测试网页。

## How to: (如何操作)
在Fish Shell中，我们可以使用外部程序，像是`pup`或`hxselect`进行HTML解析。这是一个使用`pup`解析HTML标题的例子。

```Fish Shell
echo "<html><body><h1>你好，Fish Shell！</h1></body></html>" | pup 'h1 text{}'
```

输出将会是这样的：

```plaintext
你好，Fish Shell！
```

## Deep Dive (深入了解)
HTML解析有它的历史背景。最早的网页主要是静态内容，使用正则表达式就能完成大部分任务。随着网页变得更复杂，出现了专门的解析器，如`BeautifulSoup`（Python），使得解析过程更准确也更复杂。

Fish Shell本身并不内置HTML解析功能，但通过管道命令可以很方便地调用外部程序。`pup`和`hxselect`是流行的选择，因为它们支持CSS选择器，操作直观，易于学习。

在处理复杂网页时，有时我们需要处理JavaScript渲染的内容。此时，可以使用像`puppeteer`和`Selenium`这样的工具来先渲染网页。

实施细节上，需要注意HTML的多样性和不规则性。优秀的HTML解析库不仅能处理标准的HTML文档，还能优雅地处理有错误的标记。

## See Also (另请参阅)
- [`pup` GitHub 仓库](https://github.com/ericchiang/pup)
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [BeautifulSoup 文档](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [`puppeteer` GitHub 仓库](https://github.com/puppeteer/puppeteer)
- [`Selenium` 官方网站](https://www.selenium.dev/)
