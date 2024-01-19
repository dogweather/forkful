---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么与为什么？

HTTP请求是一种让您的程序与网络服务交互的方式。程序员利用此实现如用户登录验证，取回和发送数据等功能。

## 如何做：

在PHP中，使用cURL的方法可以发送HTTP请求。下面是一个简单的示例：

```PHP
<?php
 $ch = curl_init();

curl_setopt($ch, CURLOPT_URL, "http://example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$result = curl_exec($ch);

curl_close ($ch);
?>
```

在此例中，你将看到"http://example.com"的返回结果。

## 深入研究：

早期的PHP版本并未内建支持HTTP请求，但自从PHP 4.0.2版起，cURL便成为了PHP的标准库之一。另一种可行的选择是fsockopen()函数，它更直接，更灵活，但使用复杂度更高。在发送HTTP请求时，你不仅可以设定目标URL，还可以设置请求的方法（例如GET或POST），提交的数据，以及其他诸如超时，重定向及SSL认证等参数。

## 更多信息：

如果你希望探索更多，以下链接可能有所帮助：

- PHP官方文档：[cURL](http://php.net/manual/en/book.curl.php)、[fsockopen()](http://php.net/manual/en/function.fsockopen.php)
- [Stack Overflow](https://stackoverflow.com/questions/tagged/php): 这里有大量有关PHP和HTTP请求的讨论和解答。
- [PHP中文网](http://www.php.cn/)：提供丰富的PHP学习资源和问答社区。

这只是开始，探索PHP的世界还有很多未知的乐趣等待你去发现！