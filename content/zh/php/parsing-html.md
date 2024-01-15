---
title:                "解析HTML"
html_title:           "PHP: 解析HTML"
simple_title:         "解析HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/parsing-html.md"
---

{{< edit_this_page >}}

# 为什么要解析HTML

解析HTML是指从HTML文档中提取有用的信息，并使用该信息进行数据处理或呈现。这在网页开发中非常有用，特别是当需要从多个网页中收集数据时。这可以帮助我们更快地完成日常的数据处理任务，并且可以避免手动复制和粘贴的麻烦。

## 如何解析HTML

首先，我们需要使用PHP内置的函数"file_get_contents()"来获取HTML文档的内容。接下来，我们使用PHP内置的函数"preg_match()"来匹配我们想要提取的信息，并将其存储在一个数组中。最后，我们使用循环来遍历数组，并将每个项目打印出来。

```PHP
<?php 
$html_content = file_get_contents("https://example.com");
preg_match("/<title>(.*?)<\/title>/", $html_content, $matches);
echo "网页标题是: " . $matches[1];
?>
```

**输出:**

网页标题是: example

## 深入解析HTML

除了上面介绍的基本方法外，我们还可以使用PHP库来更轻松地解析HTML文档。例如，使用PHP Simple HTML DOM Parser，我们可以通过类似于CSS选择器的语法来选择和提取HTML元素。

```PHP
<?php 
include_once 'simple_html_dom.php';
$html = file_get_html("https://example.com");
foreach($html->find('div[class=content]') as $element){
    echo $element->plaintext;
}
?>
```
**输出:**

选择的HTML元素的文本内容

此外，我们还可以使用PHP的XPath库来解析HTML文档。XPath是一种语言，可以帮助我们在HTML文档中定位和选择特定的元素。使用XPath，我们可以更精确地选择需要的元素，并提取出它们的内容。

## 参考链接

- [PHP官方文档](https://www.php.net/manual/en/book.dom.php)
- [PHP Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io)
- [PHP XPath](https://www.php.net/manual/en/book.domxpath.php)

# 请参考

- [PHP解析HTML文档教程](https://www.w3schools.com/php/php_ajax_php.asp)
- [PHP Simple HTML DOM Parser使用教程](https://simplehtmldom.sourceforge.io/manual.htm)
- [PHP XPath介绍与使用指南](https://www.php.net/manual/en/domxpath.installation.php)