---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.856699-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP\u5929\u751F\u652F\u6301\u5404\u79CD\
  \u51FD\u6570\u6765\u5927\u5199\u5B57\u7B26\u4E32\uFF0C\u6BCF\u4E2A\u51FD\u6570\u90FD\
  \u6709\u4E0D\u540C\u7684\u7528\u9014\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\u5B83\u4EEC\
  \u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:47.005210-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP\u5929\u751F\u652F\u6301\u5404\u79CD\u51FD\
  \u6570\u6765\u5927\u5199\u5B57\u7B26\u4E32\uFF0C\u6BCF\u4E2A\u51FD\u6570\u90FD\u6709\
  \u4E0D\u540C\u7684\u7528\u9014\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\u5B83\u4EEC\u7684\
  \u65B9\u6CD5\uFF1A."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

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
