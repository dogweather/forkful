---
aliases:
- /zh/php/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:44.111699-07:00
description: "\u5728PHP\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u4ECEHTML\u6587\u6863\u4E2D\
  \u63D0\u53D6\u7279\u5B9A\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\u4EFB\
  \u52A1\u4EE5\u81EA\u52A8\u5316\u6570\u636E\u63D0\u53D6\u3001\u7F51\u7EDC\u6293\u53D6\
  \uFF0C\u6216\u8005\u5728\u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u96C6\u6210\
  \u6765\u81EA\u5404\u79CD\u7F51\u9875\u7684\u5185\u5BB9\uFF0C\u65E0\u9700\u624B\u52A8\
  \u5E72\u9884\u5373\u53EF\u589E\u5F3A\u529F\u80FD\u3002"
lastmod: 2024-02-18 23:08:59.213940
model: gpt-4-0125-preview
summary: "\u5728PHP\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u4ECEHTML\u6587\u6863\u4E2D\u63D0\
  \u53D6\u7279\u5B9A\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\u4EFB\u52A1\
  \u4EE5\u81EA\u52A8\u5316\u6570\u636E\u63D0\u53D6\u3001\u7F51\u7EDC\u6293\u53D6\uFF0C\
  \u6216\u8005\u5728\u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u96C6\u6210\u6765\
  \u81EA\u5404\u79CD\u7F51\u9875\u7684\u5185\u5BB9\uFF0C\u65E0\u9700\u624B\u52A8\u5E72\
  \u9884\u5373\u53EF\u589E\u5F3A\u529F\u80FD\u3002"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PHP中解析HTML涉及从HTML文档中提取特定信息。程序员执行此任务以自动化数据提取、网络抓取，或者在他们的应用程序中集成来自各种网页的内容，无需手动干预即可增强功能。

## 如何操作：
对于解析HTML，PHP程序员可以利用内置函数或依赖强大的库，如Simple HTML DOM Parser。这里，我们将探讨使用PHP的`DOMDocument`和Simple HTML DOM Parser的示例。

### 使用`DOMDocument`：
PHP的`DOMDocument`类是其DOM扩展的一部分，允许解析和操作HTML和XML文档。这里有一个快速的示例，展示如何使用`DOMDocument`找到HTML文档中的所有图片：

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>示例页面</title>
</head>
<body>
    <img src="image1.jpg" alt="图片1">
    <img src="image2.jpg" alt="图片2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

样本输出：
```
image1.jpg
image2.jpg
```

### 使用Simple HTML DOM Parser：
对于更复杂的任务或更易用的语法，你可能更愿意使用第三方库。Simple HTML DOM Parser是一个受欢迎的选择，为导航和操作HTML结构提供了类似jQuery的界面。以下是如何使用它：

首先，使用Composer安装库：
```
composer require simple-html-dom/simple-html-dom
```

然后，操作HTML以找到所有链接，例如：

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

这段代码片段将获取“http://www.example.com”的HTML内容，解析它，并打印出所有超链接。记住将“'http://www.example.com'”替换为你希望解析的实际URL。

通过使用这些方法，PHP开发人员可以有效地解析HTML内容，根据他们的需求定制数据提取，或无缝集成外部网络内容到他们的项目中。
