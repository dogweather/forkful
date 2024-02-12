---
title:                "发出 HTTP 请求"
aliases:
- /zh/php/sending-an-http-request.md
date:                  2024-01-20T18:00:15.768089-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
发送HTTP请求让你的PHP代码能和其他服务器聊天。程序员这么做通常是为了获取数据或触发远程服务的动作。

## How to: (怎么做：)
PHP自带了几种发送HTTP请求的方法。这里我们用`curl`扩展示范：

```php
<?php
// 初始化cURL会话
$curl = curl_init();

// 设置要获取的URL
curl_setopt($curl, CURLOPT_URL, "https://api.example.com/data");

// 返回响应而非直接输出
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);

// 执行cURL请求
$response = curl_exec($curl);

// 关闭cURL资源，并且释放系统资源
curl_close($curl);

// 输出获取到的数据
echo $response;
?>
```

这段代码运行后会打印从`https://api.example.com/data`获取到的数据。

## Deep Dive (深入探讨)
发送HTTP请求的能力从PHP的早期版本就开始支持。过去用的是`fsockopen`或者`stream_context_create`，但这些方法比较复杂。现代PHP里，cURL和`file_get_contents`是最常见的做法，cURL支持更多复杂的选项和安全特性。一个更现代的选择是使用诸如Guzzle这样的第三方库，它提供了一个更优雅的API。

当使用cURL的时候，重要的是要设置正确的选项，比如：

- `CURLOPT_RETURNTRANSFER`：确保请求结果以字符串返回而不是直接输出。
- `CURLOPT_POST`和`CURLOPT_POSTFIELDS`：当需要发送POST请求和数据时使用。

正确关闭cURL资源也很重要，以避免内存泄露。

## See Also (另见)
- [PHP cURL Documentation](https://www.php.net/manual/en/book.curl.php)
- [Guzzle, PHP HTTP Client](http://docs.guzzlephp.org/en/stable/)
- [PHP Stream Contexts](https://www.php.net/manual/en/context.php)
