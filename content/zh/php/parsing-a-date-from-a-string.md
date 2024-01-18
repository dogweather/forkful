---
title:                "从字符串中分析日期"
html_title:           "PHP: 从字符串中分析日期"
simple_title:         "从字符串中分析日期"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
解析日期字符串是指将一个字符串中的日期信息提取出来，以便程序员可以对其进行进一步的操作和处理。程序员通常需要解析日期字符串是因为它们从不同的来源（例如用户输入或数据库）获取的日期格式可能不一致，需要经过解析才能统一格式。

## 如何：
```PHP
$str = "2021年7月2日";
$date = date("Y-m-d", strtotime($str));
echo $date; // 输出: 2021-07-02
```

```PHP
$str = "7/2/2021";
$date = DateTime::createFromFormat("m/d/Y", $str)->format("Y-m-d");
echo $date; // 输出: 2021-07-02
```

## 深入了解：
1. 历史背景：在早期的编程语言中，日期的处理受限于计算机的能力，因此需要借助解析字符串的方法来处理日期。随着计算机技术的进步，现在的编程语言通常都内置了更方便的日期处理函数，但解析日期字符串仍然是一个重要的技能。
2. 替代方法：除了使用PHP内置的日期处理函数，还可以使用第三方的日期解析库，例如Carbon。
3. 实现细节：在解析日期字符串时，程序会先将字符串转换成时间戳，再根据需求将时间戳转换成特定的日期格式。因此，如果字符串中的日期信息格式错误，很可能导致解析失败。

## 参考资料：
- PHP日期处理手册：https://www.php.net/manual/zh/book.datetime.php
- Carbon日期处理库：https://carbon.nesbot.com/