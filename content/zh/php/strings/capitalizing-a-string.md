---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.856699-07:00
description: "\u5B57\u7B26\u4E32\u5927\u5199\u5316\u6D89\u53CA\u5C06\u7ED9\u5B9A\u6587\
  \u672C\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4FEE\u6539\u4E3A\u5927\u5199\uFF0C\u786E\
  \u4FDD\u6570\u636E\u96C6\u4E2D\u7684\u53E5\u5B50\u3001\u6807\u9898\u6216\u4E13\u6709\
  \u540D\u8BCD\u6B63\u786E\u5F00\u59CB\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\
  \u5B57\u7B26\u4E32\u5927\u5199\u5316\u4EE5\u7528\u4E8E\u6570\u636E\u6807\u51C6\u5316\
  \uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u786E\u4FDD\u7528\u6237\u8F93\u5165\u6216\
  \u6587\u672C\u6570\u636E\u5904\u7406\u7684\u4E00\u81F4\u6027\u3002"
lastmod: '2024-03-11T00:14:21.633175-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u5927\u5199\u5316\u6D89\u53CA\u5C06\u7ED9\u5B9A\u6587\
  \u672C\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4FEE\u6539\u4E3A\u5927\u5199\uFF0C\u786E\
  \u4FDD\u6570\u636E\u96C6\u4E2D\u7684\u53E5\u5B50\u3001\u6807\u9898\u6216\u4E13\u6709\
  \u540D\u8BCD\u6B63\u786E\u5F00\u59CB\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\
  \u5B57\u7B26\u4E32\u5927\u5199\u5316\u4EE5\u7528\u4E8E\u6570\u636E\u6807\u51C6\u5316\
  \uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\u6216\u786E\u4FDD\u7528\u6237\u8F93\u5165\u6216\
  \u6587\u672C\u6570\u636E\u5904\u7406\u7684\u4E00\u81F4\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
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
