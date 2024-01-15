---
title:                "下载网页"
html_title:           "PHP: 下载网页"
simple_title:         "下载网页"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

为什么有人会想要下载一个网页呢？这可能是因为他们想要保存网页内容，以便以后参考或离线浏览。另外，网页下载也是一个网站测试和网页抓取的重要步骤。

## 如何？

```PHP
<?php
$url = 'https://www.example.com';
$output_file = 'example.html';

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$html = curl_exec($ch);
curl_close($ch);

file_put_contents($output_file, $html);

echo '网页已成功下载并保存在' . $output_file . '文件中。';
```

输出：

```
网页已成功下载并保存在example.html文件中。
```

## 深入了解

网页下载是通过使用一种叫做CURL的库来实现的。CURL是一个通用的用于传输数据的库，它支持各种协议，包括HTTP，HTTPS，FTP等。通过使用CURL，我们可以轻松地设置网页下载的选项，如URL，保存路径，请求头等。

## 参考链接

- [PHP官方CURL文档](https://www.php.net/manual/en/book.curl.php)
- [CURL官方文档](https://curl.haxx.se/docs/manual.html)
- [PHP教程-使用CURL下载网页](https://www.php.net/manual/en/book.curl.php)