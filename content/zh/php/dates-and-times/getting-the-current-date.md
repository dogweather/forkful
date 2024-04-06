---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:10.088924-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP\u5185\u7F6E\u7684`date()`\u51FD\u6570\
  \u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6700\u76F4\u63A5\u7684\u65B9\u6CD5\u3002\
  \u60A8\u53EF\u4EE5\u901A\u8FC7\u6307\u5B9A\u683C\u5F0F\u53C2\u6570\u4EE5\u5404\u79CD\
  \u65B9\u5F0F\u683C\u5F0F\u5316\u65E5\u671F\u3002"
lastmod: '2024-03-13T22:44:47.875651-06:00'
model: gpt-4-0125-preview
summary: "PHP\u5185\u7F6E\u7684`date()`\u51FD\u6570\u662F\u83B7\u53D6\u5F53\u524D\u65E5\
  \u671F\u6700\u76F4\u63A5\u7684\u65B9\u6CD5\u3002\u60A8\u53EF\u4EE5\u901A\u8FC7\u6307\
  \u5B9A\u683C\u5F0F\u53C2\u6570\u4EE5\u5404\u79CD\u65B9\u5F0F\u683C\u5F0F\u5316\u65E5\
  \u671F."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：


### 原生 PHP
PHP内置的`date()`函数是获取当前日期最直接的方法。您可以通过指定格式参数以各种方式格式化日期。

```php
echo date("Y-m-d"); // 输出：2023-04-01（例如）
echo date("l, F j, Y"); // 输出：Saturday, April 1, 2023
```

要获取有时区支持的日期和时间，您可以使用`DateTime`类配合`DateTimeZone`。

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // 输出：2023-04-01 12:00:00（例如）
```

### 使用 Carbon（一个流行的第三方库）
[Carbon](https://carbon.nesbot.com/) 是一个简单的 `DateTime` API 扩展，为日期和时间的操作提供了更清晰、更流畅的方式。

首先，确保通过 Composer 安装了 Carbon：
```bash
composer require nesbot/carbon
```

然后，您可以使用它来获取当前日期：

```php
use Carbon\Carbon;

echo Carbon::now(); // 输出：2023-04-01 12:00:00（例如，默认格式）
echo Carbon::now()->toDateString(); // 输出：2023-04-01
echo Carbon::now()->format('l, F j, Y'); // 输出：Saturday, April 1, 2023
```

Carbon 通过增加可读性和大量时间操作、比较、格式化的功能，丰富了PHP中的日期时间处理。
