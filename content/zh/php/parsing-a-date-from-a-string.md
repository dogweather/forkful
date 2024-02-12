---
title:                "从字符串解析日期"
aliases:
- zh/php/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:55.505760-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
