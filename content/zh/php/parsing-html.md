---
title:                "解析HTML"
date:                  2024-01-20T15:33:09.346015-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "PHP"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
解析HTML就是从一堆看似无序代码里提取有用信息。程序员这么做通常是为了自动化地处理网页数据，比如抓取、数据挖掘。

## How to: (如何做？)
```PHP
// 确保你安装了DOMDocument拓展
$doc = new DOMDocument();
@$doc->loadHTML(file_get_contents('https://example.com'));
$xpath = new DOMXPath($doc);

// 获取所有<h1>标签内容
$h1s = $xpath->query('//h1');
foreach($h1s as $h1) {
    echo $h1->nodeValue . PHP_EOL;
}

// 查询class为"important"的div
$importantDivs = $xpath->query('//div[@class="important"]');
foreach($importantDivs as $div) {
    echo $div->nodeValue . PHP_EOL;
}
```
输出会显示你查询的所有 `<h1>` 标签和类名为 "important" 的 `<div>` 标签的内容。

## Deep Dive (深入研究)
在早期，PHP程序员经常使用正则表达式来解析HTML，但这种方法易错且维护困难。DOMDocument 和 DOMXPath 出现后，提供了一个更强大和标准化的方式来解析HTML。这不仅更可靠，而且使得代码易于理解和维护。有时候，开发者可能会使用第三方库如 `Simple HTML DOM Parser`，但内置的 DOM 扩展在性能和兼容性上通常更胜一筹。相对来说，使用 DOMXPath 进行查询，可以实现更复杂的文档搜索和处理，尤其是当处理有特定结构的HTML时。

## See Also (另请参阅)
- [DOMDocument](https://www.php.net/manual/zh/class.domdocument.php)
- [DOMXPath](https://www.php.net/manual/zh/class.domxpath.php)
- [Simple HTML DOM Parser](http://simplehtmldom.sourceforge.net/)
