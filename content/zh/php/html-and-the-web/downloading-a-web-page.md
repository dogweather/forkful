---
title:                "下载网页"
aliases:
- /zh/php/downloading-a-web-page/
date:                  2024-01-20T17:44:32.244239-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/downloading-a-web-page.md"
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
