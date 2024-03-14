---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:20.790331-07:00
description: "\u5728PHP\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u5C31\u50CF\u52A0\u5F3A\
  \u7248\u7684\u5217\u8868\uFF0C\u6BCF\u4E2A\u5143\u7D20\u90FD\u53EF\u4EE5\u901A\u8FC7\
  \u4E00\u4E2A\u4EBA\u7C7B\u53EF\u8BFB\u7684\u952E\u800C\u4E0D\u4EC5\u4EC5\u662F\u6570\
  \u5B57\u6765\u8BBF\u95EE\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u66F4\
  \u76F4\u89C2\u5730\u5B58\u50A8\u548C\u64CD\u7EB5\u6570\u636E\uFF0C\u4ECE\u800C\u4F7F\
  \u4EE3\u7801\u66F4\u6613\u4E8E\u9605\u8BFB\u548C\u7EF4\u62A4\u3002"
lastmod: '2024-03-13T22:44:47.855685-06:00'
model: gpt-4-0125-preview
summary: "\u5728PHP\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u5C31\u50CF\u52A0\u5F3A\u7248\
  \u7684\u5217\u8868\uFF0C\u6BCF\u4E2A\u5143\u7D20\u90FD\u53EF\u4EE5\u901A\u8FC7\u4E00\
  \u4E2A\u4EBA\u7C7B\u53EF\u8BFB\u7684\u952E\u800C\u4E0D\u4EC5\u4EC5\u662F\u6570\u5B57\
  \u6765\u8BBF\u95EE\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u66F4\u76F4\
  \u89C2\u5730\u5B58\u50A8\u548C\u64CD\u7EB5\u6570\u636E\uFF0C\u4ECE\u800C\u4F7F\u4EE3\
  \u7801\u66F4\u6613\u4E8E\u9605\u8BFB\u548C\u7EF4\u62A4\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么以及为什么？

在PHP中，关联数组就像加强版的列表，每个元素都可以通过一个人类可读的键而不仅仅是数字来访问。程序员使用它们来更直观地存储和操纵数据，从而使代码更易于阅读和维护。

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
