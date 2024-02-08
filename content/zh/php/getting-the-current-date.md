---
title:                "获取当前日期"
aliases:
- zh/php/getting-the-current-date.md
date:                  2024-02-03T19:10:10.088924-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PHP中获取当前日期是一个基本任务，它允许您检索和操作系统的日期和时间。这对于记录、时间戳记帖子、安排事件或在应用程序中执行时间敏感操作等功能至关重要。

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
