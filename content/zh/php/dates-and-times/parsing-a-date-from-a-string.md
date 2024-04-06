---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:55.505760-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP\u5185\u7F6E\u7684`DateTime`\u7C7B\
  \u63D0\u4F9B\u4E86\u4E00\u5957\u5F3A\u5927\u7684\u51FD\u6570\u7528\u4E8E\u89E3\u6790\
  \u548C\u5904\u7406\u65E5\u671F\u3002\u60A8\u53EF\u4EE5\u4F7F\u7528\u6784\u9020\u51FD\
  \u6570\u4ECE\u65E5\u671F\u5B57\u7B26\u4E32\u521B\u5EFA\u4E00\u4E2A`DateTime`\u5B9E\
  \u4F8B\uFF0C\u7136\u540E\u6839\u636E\u9700\u8981\u5BF9\u5176\u8FDB\u884C\u683C\u5F0F\
  \u5316\u3002\u65B9\u6CD5\u5982\u4E0B\uFF1A."
lastmod: '2024-04-05T21:53:48.181974-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
PHP内置的`DateTime`类提供了一套强大的函数用于解析和处理日期。您可以使用构造函数从日期字符串创建一个`DateTime`实例，然后根据需要对其进行格式化。方法如下：

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 输出：2023-04-25 15:30:00
```

为了处理遵循非标准格式的字符串，您可以使用`createFromFormat`方法，该方法允许您指定输入日期的确切格式：

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 输出：2023-04-25 15:30:00
```

对于可能不被`DateTime`直接支持的更复杂的解析，PHP提供了`strtotime`函数，它试图将任何英文文本日期时间描述解析为Unix时间戳：

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// 输出将根据当前日期变化，例如："2023-05-04"
```

**使用第三方库：**

虽然PHP的内置函数涵盖了广泛的用例，但有时您可能需要更复杂的解析功能。Carbon库是PHP的DateTime类的扩展，提供了丰富的日期/时间操作特性：

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// 输出将变化，例如："2023-04-26 00:00:00"
```

Carbon的`parse`方法可以智能处理多种日期和时间格式，使其成为需要灵活日期解析功能的应用程序的宝贵工具。
