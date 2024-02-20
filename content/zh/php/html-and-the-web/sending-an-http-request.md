---
date: 2024-01-20 18:00:15.768089-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u8BA9\u4F60\u7684PHP\u4EE3\u7801\u80FD\u548C\
  \u5176\u4ED6\u670D\u52A1\u5668\u804A\u5929\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u901A\u5E38\u662F\u4E3A\u4E86\u83B7\u53D6\u6570\u636E\u6216\u89E6\u53D1\u8FDC\u7A0B\
  \u670D\u52A1\u7684\u52A8\u4F5C\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.901756
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u8BA9\u4F60\u7684PHP\u4EE3\u7801\u80FD\u548C\
  \u5176\u4ED6\u670D\u52A1\u5668\u804A\u5929\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u901A\u5E38\u662F\u4E3A\u4E86\u83B7\u53D6\u6570\u636E\u6216\u89E6\u53D1\u8FDC\u7A0B\
  \u670D\u52A1\u7684\u52A8\u4F5C\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
