---
date: 2024-01-20 17:44:32.244239-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u4E3A\u4E86\u5206\u6790\u7F51\u9875\u6570\u636E\u3001\u76D1\u63A7\u5185\u5BB9\u53D8\
  \u5316\u6216\u8005\u5F52\u6863\u5B58\u50A8\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.862226-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\
  \u4E3A\u4E86\u5206\u6790\u7F51\u9875\u6570\u636E\u3001\u76D1\u63A7\u5185\u5BB9\u53D8\
  \u5316\u6216\u8005\u5F52\u6863\u5B58\u50A8\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
下载网页就是从互联网上获取网页的内容。程序员这么做是为了分析网页数据、监控内容变化或者归档存储。

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
