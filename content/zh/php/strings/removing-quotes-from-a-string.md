---
date: 2024-01-26 03:41:14.724144-07:00
description: "\u5728PHP\u4E2D\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\
  \u610F\u5473\u7740\u53BB\u9664\u90A3\u4E9B\u53EF\u80FD\u5E72\u6270\u4F60\u7684\u4EE3\
  \u7801\u903B\u8F91\u6216\u6570\u636E\u5E93\u67E5\u8BE2\u7684\u8BA8\u538C\u7684\u53CC\
  \u5F15\u53F7\uFF08`\"`\uFF09\u6216\u5355\u5F15\u53F7\uFF08`'`\uFF09\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u6216\u6D88\u6BD2\u8F93\u5165\
  \u6570\u636E\uFF0C\u786E\u4FDD\u5B57\u7B26\u4E32\u53EF\u4EE5\u5B89\u5168\u4F7F\u7528\
  \u6216\u5B58\u50A8\u3002"
lastmod: 2024-02-19 22:05:06.890912
model: gpt-4-0125-preview
summary: "\u5728PHP\u4E2D\u79FB\u9664\u5B57\u7B26\u4E32\u4E2D\u7684\u5F15\u53F7\u610F\
  \u5473\u7740\u53BB\u9664\u90A3\u4E9B\u53EF\u80FD\u5E72\u6270\u4F60\u7684\u4EE3\u7801\
  \u903B\u8F91\u6216\u6570\u636E\u5E93\u67E5\u8BE2\u7684\u8BA8\u538C\u7684\u53CC\u5F15\
  \u53F7\uFF08`\"`\uFF09\u6216\u5355\u5F15\u53F7\uFF08`'`\uFF09\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6E05\u7406\u6216\u6D88\u6BD2\u8F93\u5165\u6570\
  \u636E\uFF0C\u786E\u4FDD\u5B57\u7B26\u4E32\u53EF\u4EE5\u5B89\u5168\u4F7F\u7528\u6216\
  \u5B58\u50A8\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PHP中移除字符串中的引号意味着去除那些可能干扰你的代码逻辑或数据库查询的讨厌的双引号（`"`）或单引号（`'`）。程序员这样做是为了清理或消毒输入数据，确保字符串可以安全使用或存储。

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
