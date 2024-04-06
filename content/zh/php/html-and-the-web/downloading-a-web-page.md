---
date: 2024-01-20 17:44:32.244239-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.071300-06:00'
model: gpt-4-1106-preview
summary: "How to (\u5982\u4F55\u64CD\u4F5C) \u65E9\u671F\uFF0C\u4F7F\u7528`file_get_contents()`\u5C31\
  \u53EF\u4EE5\u4E0B\u8F7D\u7F51\u9875\uFF0C\u4F46\u8FD9\u79CD\u65B9\u6CD5\u5BF9\u4E8E\
  \u5904\u7406HTTP\u8BF7\u6C42\u7684\u63A7\u5236\u4E0D\u591F\u7075\u6D3B\u3002\u56E0\
  \u6B64\uFF0C`cURL`\u5E93\u6210\u4E86\u4E3B\u6D41\u9009\u62E9\u3002cURL\u652F\u6301\
  \u591A\u79CD\u534F\u8BAE\uFF0C\u53EF\u4EE5\u8BBE\u7F6E\u4EE3\u7406\u3001HTTP\u5934\
  \u548Ccookies\u7B49\u3002`fsockopen()`\u548C`fopen()`\u5728\u67D0\u4E9B\u65E7\u4EE3\
  \u7801\u4E2D\u8FD8\u80FD\u89C1\u5230\uFF0C\u4F46\u5B83\u4EEC\u4E0D\u5982cURL\u5F3A\
  \u5927\u3002\u5B9E\u65BD\u7EC6\u8282\u4E0A\uFF0C\u4F7F\u7528cURL\u65F6\uFF0C\u901A\
  \u8FC7`curl_setopt()`\u51FD\u6570\u53EF\u4EE5\u7EC6\u81F4\u5730\u8BBE\u7F6E\u8BF7\
  \u6C42\u53C2\u6570\uFF0C\u4EE5\u5E94\u5BF9\u4E0D\u540C\u7684\u7F51\u7EDC\u60C5\u51B5\
  \u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## How to (如何操作)
使用PHP下载网页简单快捷。这里有个例子：

```php
<?php
$url = "http://example.com";
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($ch);
curl_close($ch);

echo $pageContent;
?>
```

样本输出（视网页内容而定）：

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive (深入了解)
早期，使用`file_get_contents()`就可以下载网页，但这种方法对于处理HTTP请求的控制不够灵活。因此，`cURL`库成了主流选择。cURL支持多种协议，可以设置代理、HTTP头和cookies等。`fsockopen()`和`fopen()`在某些旧代码中还能见到，但它们不如cURL强大。实施细节上，使用cURL时，通过`curl_setopt()`函数可以细致地设置请求参数，以应对不同的网络情况。

## See Also (另请参见)
- [PHP cURL 官方文档](https://www.php.net/manual/en/book.curl.php)
- [PHP `file_get_contents` 官方文档](https://www.php.net/manual/en/function.file-get-contents.php)
- [Stack Overflow: PHP文件下载](https://stackoverflow.com/questions/tagged/php+file-download)
