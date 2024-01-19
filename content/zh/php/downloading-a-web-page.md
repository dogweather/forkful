---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

下载网页是指将网页的源代码从服务器获取并储存在本地的过程。程序员通常执行此操作以获取和处理网页上的数据—例如，进行网络爬虫或者数据抓取。

## 怎么做？

在PHP中下载网页内容的一种简单方法是使用`file_get_contents()`函数。如下的代码将下载网页并输出其内容：

```PHP
<?php
$web_page = file_get_contents('http://example.com');
echo $web_page;
?>
```

在运行此代码后，会看到 example.com 的源代码。这就是所谓的“下载网页”。

## 深入探索

`file_get_contents()`函数是从PHP 4.3.0版本开始引入的，至今仍然被广泛使用。但是，这个函数有其局限性，例如缺乏HTTP协议高级特性的支持，和安全性问题。因此，许多开发者会选择使用其他方法进行网络操作，如CURL和stream_context。

CURL（Client URL）是一个由Daniel Stenberg创建的开源库，能够通过各种协议连接和通信，包括 HTTP。 在PHP中，可以通过提供的CURL接口来使用这个库，如下所示：

```PHP
<?php
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'http://example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$web_page = curl_exec($ch);
curl_close($ch);
echo $web_page;
?>
```

另一种解决方案是使用流环境（stream_context）。这也是一个从PHP 4.3.0版本引入的特性，可以设置和处理各种数据流的上下文，包括HTTP请求。

```PHP
<?php
$context = stream_context_create(['http' => ['method'=>'GET']]);
$web_page = file_get_contents('http://example.com', false, $context);
echo $web_page;
?>
```

## 另请参见

* PHP官方在线手册的 `file_get_contents` 页面: https://www.php.net/manual/en/function.file-get-contents.php
* CURL的 PHP 手册页面：https://www.php.net/manual/en/book.curl.php
* `stream_context_create` 的 PHP 手册页面：https://www.php.net/manual/en/function.stream-context-create.php