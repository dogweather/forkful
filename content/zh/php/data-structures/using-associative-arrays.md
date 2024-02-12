---
title:                "使用关联数组"
aliases: - /zh/php/using-associative-arrays.md
date:                  2024-01-30T19:12:20.790331-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
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
