---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:36.055373-07:00
description: "JSON\uFF0C\u5373JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF0C\u662F\
  \u4E00\u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5B83\
  \u5BF9\u4EBA\u7C7B\u6765\u8BF4\u6613\u4E8E\u8BFB\u5199\uFF0C\u5BF9\u673A\u5668\u6765\
  \u8BF4\u6613\u4E8E\u89E3\u6790\u548C\u751F\u6210\u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\
  \u4F7F\u7528JSON\u5728\u670D\u52A1\u5668\u548CWeb\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\
  \u4EA4\u6362\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u548C\u8BED\
  \u8A00\u65E0\u5173\u6027\uFF0C\u4F7F\u5F97\u5B83\u6210\u4E3A\u73B0\u4EE3Web\u5F00\
  \u53D1\u548CAPIs\u4E2D\u7684\u57FA\u77F3\u3002"
lastmod: '2024-03-11T00:14:21.679546-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF0C\u5373JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF0C\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5B83\u5BF9\
  \u4EBA\u7C7B\u6765\u8BF4\u6613\u4E8E\u8BFB\u5199\uFF0C\u5BF9\u673A\u5668\u6765\u8BF4\
  \u6613\u4E8E\u89E3\u6790\u548C\u751F\u6210\u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u4F7F\
  \u7528JSON\u5728\u670D\u52A1\u5668\u548CWeb\u5E94\u7528\u7A0B\u5E8F\u4E4B\u95F4\u4EA4\
  \u6362\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u548C\u8BED\u8A00\
  \u65E0\u5173\u6027\uFF0C\u4F7F\u5F97\u5B83\u6210\u4E3A\u73B0\u4EE3Web\u5F00\u53D1\
  \u548CAPIs\u4E2D\u7684\u57FA\u77F3\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么？
JSON，即JavaScript对象表示法，是一种轻量级的数据交换格式，它对人类来说易于读写，对机器来说易于解析和生成。程序员常常使用JSON在服务器和Web应用程序之间交换数据，因为它的简单性和语言无关性，使得它成为现代Web开发和APIs中的基石。

## 如何操作：
在PHP中处理JSON非常直接，多亏了内置函数`json_encode()`和`json_decode()`。以下是将PHP数组转换成JSON字符串，以及反之亦然的示例：

### 将PHP数组编码成JSON字符串
```php
// 定义一个关联数组
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// 将PHP数组转换为JSON字符串
$jsonString = json_encode($data);

// 输出JSON字符串
echo $jsonString;
```
**样例输出：**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### 将JSON字符串解码为PHP数组
```php
// JSON字符串
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// 将JSON字符串转换为PHP数组
$data = json_decode($jsonString, true);

// 输出PHP数组
print_r($data);
```
**样例输出：**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### 使用第三方库：GuzzleHttp
对于复杂的JSON和Web请求处理，一个受欢迎的PHP库是GuzzleHttp。它简化了HTTP请求，并能轻松处理JSON数据。

**通过Composer安装：**
```
composer require guzzlehttp/guzzle
```

**示例请求：**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// 向一个返回JSON的API发送请求
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// 将JSON响应解码为PHP数组
$data = json_decode($response->getBody(), true);

// 输出数据
print_r($data);
```

**假设API返回类似的JSON数据：**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
这展示了使用PHP进行JSON操作的便捷之处，无论是使用原生函数还是使用如GuzzleHttp这样的强大库处理更复杂的任务。
