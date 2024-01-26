---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:41:14.724144-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/removing-quotes-from-a-string.md"
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