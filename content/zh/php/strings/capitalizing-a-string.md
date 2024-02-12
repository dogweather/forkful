---
title:                "字符串大写化"
aliases: - /zh/php/capitalizing-a-string.md
date:                  2024-02-03T19:05:56.856699-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串大写化涉及将给定文本的第一个字符修改为大写，确保数据集中的句子、标题或专有名词正确开始。程序员经常执行字符串大写化以用于数据标准化，提高可读性或确保用户输入或文本数据处理的一致性。

## 如何操作：
PHP天生支持各种函数来大写字符串，每个函数都有不同的用途。以下是使用它们的方法：

### 大写字符串的第一个字母：

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // 输出：Hello, world!
```

### 大写每个单词的第一个字母：

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // 输出：Hello, World!
```

### 将整个字符串转换为大写：

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // 输出：HELLO, WORLD!
```

对于需要更多自定义或第三方解决方案的场景，可以利用诸如`mbstring`（用于多字节字符串）的库，特别是在处理国际化时，字符可能超出基本ASCII集。

### 使用mbstring大写UTF-8字符串：

确保你的PHP配置中启用了`mbstring`扩展，然后：

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // 输出：Élégant
```

这种方法有助于准确大写包含非ASCII字符的字符串，遵守各种语言的细微差别。
