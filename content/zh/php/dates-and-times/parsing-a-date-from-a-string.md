---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:55.505760-07:00
description: "\u5728PHP\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u5C06\u8868\u793A\u65E5\u671F\u548C/\u6216\u65F6\u95F4\u7684\u6587\u672C\
  \u8F6C\u6362\u4E3APHP `DateTime`\u5BF9\u8C61\u6216\u5176\u4ED6\u65E5\u671F/\u65F6\
  \u95F4\u683C\u5F0F\u3002\u8FD9\u5BF9\u4E8E\u6570\u636E\u9A8C\u8BC1\u3001\u64CD\u4F5C\
  \u3001\u5B58\u50A8\u548C\u5C55\u793A\u7279\u522B\u91CD\u8981\uFF0C\u5C24\u5176\u662F\
  \u5728\u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u6765\u81EA\u5916\u90E8\u6765\u6E90\
  \u7684\u6570\u636E\u65F6\u3002"
lastmod: '2024-02-25T18:49:45.442314-07:00'
model: gpt-4-0125-preview
summary: "\u5728PHP\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u6D89\
  \u53CA\u5C06\u8868\u793A\u65E5\u671F\u548C/\u6216\u65F6\u95F4\u7684\u6587\u672C\u8F6C\
  \u6362\u4E3APHP `DateTime`\u5BF9\u8C61\u6216\u5176\u4ED6\u65E5\u671F/\u65F6\u95F4\
  \u683C\u5F0F\u3002\u8FD9\u5BF9\u4E8E\u6570\u636E\u9A8C\u8BC1\u3001\u64CD\u4F5C\u3001\
  \u5B58\u50A8\u548C\u5C55\u793A\u7279\u522B\u91CD\u8981\uFF0C\u5C24\u5176\u662F\u5728\
  \u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u6765\u81EA\u5916\u90E8\u6765\u6E90\u7684\
  \u6570\u636E\u65F6\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在PHP中解析字符串中的日期涉及将表示日期和/或时间的文本转换为PHP `DateTime`对象或其他日期/时间格式。这对于数据验证、操作、存储和展示特别重要，尤其是在处理用户输入或来自外部来源的数据时。

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
