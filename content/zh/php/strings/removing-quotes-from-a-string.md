---
date: 2024-01-26 03:41:14.724144-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728PHP\u7684\u65E9\u671F\uFF0C\u5F00\
  \u53D1\u8005\u5FC5\u987B\u5BF9\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\u683C\u5916\
  \u5C0F\u5FC3\uFF0C\u7279\u522B\u662F\u5728\u63D2\u5165\u6570\u636E\u5E93\u7684\u6570\
  \u636E\u65F6\u3002\u5904\u7406\u4E0D\u5F53\u7684\u5F15\u53F7\u53EF\u80FD\u5BFC\u81F4\
  SQL\u6CE8\u5165\u653B\u51FB\u3002\u4E8E\u662F\u5F15\u5165\u4E86\u9B54\u672F\u5F15\
  \u53F7\uFF0C\u4E00\u4E2A\u81EA\u52A8\u8F6C\u4E49\u8F93\u5165\u6570\u636E\u7684\u7279\
  \u6027\u3002\u540E\u6765\uFF0C\u5B83\u56E0\u4E3A\u9F13\u52B1\u4E86\u4E0D\u826F\u7F16\
  \u7A0B\u4E60\u60EF\u548C\u5B89\u5168\u95EE\u9898\u800C\u88AB\u5F03\u7528\u5E76\u6700\
  \u7EC8\u79FB\u9664\u3002\u2026"
lastmod: '2024-04-05T22:51:01.059196-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728PHP\u7684\u65E9\u671F\uFF0C\u5F00\u53D1\
  \u8005\u5FC5\u987B\u5BF9\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\u683C\u5916\u5C0F\
  \u5FC3\uFF0C\u7279\u522B\u662F\u5728\u63D2\u5165\u6570\u636E\u5E93\u7684\u6570\u636E\
  \u65F6\u3002\u5904\u7406\u4E0D\u5F53\u7684\u5F15\u53F7\u53EF\u80FD\u5BFC\u81F4SQL\u6CE8\
  \u5165\u653B\u51FB\u3002\u4E8E\u662F\u5F15\u5165\u4E86\u9B54\u672F\u5F15\u53F7\uFF0C\
  \u4E00\u4E2A\u81EA\u52A8\u8F6C\u4E49\u8F93\u5165\u6570\u636E\u7684\u7279\u6027\u3002\
  \u540E\u6765\uFF0C\u5B83\u56E0\u4E3A\u9F13\u52B1\u4E86\u4E0D\u826F\u7F16\u7A0B\u4E60\
  \u60EF\u548C\u5B89\u5168\u95EE\u9898\u800C\u88AB\u5F03\u7528\u5E76\u6700\u7EC8\u79FB\
  \u9664\u3002 \u73B0\u5728\uFF0C\u6211\u4EEC\u4F7F\u7528\u50CF`str_replace()`\u6216\
  \u8005\u7528`preg_replace()`\u8FDB\u884C\u66F4\u9AD8\u7EA7\u6A21\u5F0F\u5339\u914D\
  \u7684\u6B63\u5219\u8868\u8FBE\u5F0F\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u6B63\u5219\
  \u8868\u8FBE\u5F0F\u7684\u4F8B\u5B50\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作：
这里有一个使用PHP内置函数的简单例子：

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // 输出：Hello, she said, Its a fine day!
```

简单吧？这个`str_replace()`函数接受一个字符数组从字符串中移除，包括单引号和双引号。

## 深入探讨
在PHP的早期，开发者必须对字符串中的引号格外小心，特别是在插入数据库的数据时。处理不当的引号可能导致SQL注入攻击。于是引入了魔术引号，一个自动转义输入数据的特性。后来，它因为鼓励了不良编程习惯和安全问题而被弃用并最终移除。

现在，我们使用像`str_replace()`或者用`preg_replace()`进行更高级模式匹配的正则表达式。这里有一个正则表达式的例子：

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

对于JSON数据，你可能会使用`json_encode()`，并且选择像`JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`这样的选项来避免在你的引号中出现额外的反斜杠。

实施时，考虑边缘情况。如果你的字符串本意就包含某些特定的引号怎么办，比如说故事中的对话或是测量中的英寸标记？上下文很重要，所以要根据数据的预期用途来定制你的去引号处理。

## 另见
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL注入预防](https://owasp.org/www-community/attacks/SQL_Injection)
