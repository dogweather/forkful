---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:20.790331-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP\u4E2D\u7684\u5173\u8054\u6570\u7EC4\
  \u5185\u90E8\u4F7F\u7528\u54C8\u5E0C\u8868\u5B9E\u73B0\uFF0C\u8FD9\u4E3A\u901A\u8FC7\
  \u952E\u5FEB\u901F\u8BBF\u95EE\u5143\u7D20\u63D0\u4F9B\u4E86\u975E\u5E38\u5FEB\u7684\
  \u901F\u5EA6\uFF0C\u4F7F\u5B83\u4EEC\u5728\u8BB8\u591A\u4EFB\u52A1\u4E2D\u975E\u5E38\
  \u9AD8\u6548\u3002\u8FD9\u79CD\u6548\u7387\uFF0C\u52A0\u4E0A\u5B83\u4EEC\u7684\u6613\
  \u7528\u6027\uFF0C\u4F7F\u5F97\u5173\u8054\u6570\u7EC4\u6210\u4E3APHP\u7F16\u7A0B\
  \u7684\u57FA\u77F3\u3002\u2026"
lastmod: '2024-04-05T22:51:01.065019-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5386\u53F2\u4E0A\u770B\uFF0CPHP\u7684\u6570\u7EC4\uFF08\u65E0\u8BBA\
  \u662F\u7D22\u5F15\u6570\u7EC4\u8FD8\u662F\u5173\u8054\u6570\u7EC4\uFF09\u90FD\u975E\
  \u5E38\u7075\u6D3B\uFF0C\u5141\u8BB8\u5B83\u4EEC\u5145\u5F53\u5217\u8868\u3001\u5806\
  \u6808\u3001\u961F\u5217\u7B49\u591A\u79CD\u89D2\u8272\u3002\u7136\u800C\uFF0C\u8FD9\
  \u79CD\u7075\u6D3B\u6027\u5982\u679C\u4E0D\u5C0F\u5FC3\u4F7F\u7528\uFF0C\u6709\u65F6\
  \u4F1A\u5BFC\u81F4\u4EE3\u7801\u6548\u7387\u964D\u4F4E\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作：
在PHP中，创建和使用关联数组非常直接。这里是一个快速概述：

```PHP
<?php
// 创建一个关联数组
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// 或者，使用简短数组语法
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// 使用键访问值
echo "Name: " . $person["name"] . "\n";
echo "Age: " . $person["age"] . "\n";
echo "Email: " . $person["email"] . "\n";

// 修改一个值
$person["age"] = 31;

// 添加一个新的键值对
$person["country"] = "USA";

// 遍历关联数组
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// 输出
// Name: John Doe
// Age: 31
// Email: john@example.com
// country: USA
?>
```

注意，键可以是任何字符串，这允许你使用这些键而不是数字索引来访问元素，这可能意义不大且难以记住。

## 深入了解
PHP中的关联数组内部使用哈希表实现，这为通过键快速访问元素提供了非常快的速度，使它们在许多任务中非常高效。这种效率，加上它们的易用性，使得关联数组成为PHP编程的基石。

从历史上看，PHP的数组（无论是索引数组还是关联数组）都非常灵活，允许它们充当列表、堆栈、队列等多种角色。然而，这种灵活性如果不小心使用，有时会导致代码效率降低。

最近，随着PHP面向对象编程的改进，一些开发者更倾向于使用对象来处理结构化数据，特别是对于复杂或相互关联的数据集。使用类可以提供更好的封装和抽象，使代码更易于测试，并明确意图。然而，对于简单的键值存储和直接的数据操纵场景，关联数组仍然是一个优秀的选择，因为它们的简单性和直观的语法。
