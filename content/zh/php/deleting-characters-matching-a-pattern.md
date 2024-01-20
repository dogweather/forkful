---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何为何？(What & Why?)

PHP中删除匹配模式的字符涉及找出字符串中符合特定规则（模式）的字符，并将其删除。程序员这样做主要是为了对数据进行清理或者格式化，如删除非数字字符，使得输入更加规范。

## 如何实现？(How to?)

在PHP中我们可以使用`preg_replace()`函数来删除匹配特定模式的字符。请看下面的例子：

```PHP
<?php
$text = "Hello, 123 world!456";
$pattern = '/\D/';
$result = preg_replace($pattern, "", $text);
echo $result;
?>
```

运行此代码，你将看到以下输出：

```PHP
123456
```
在上述代码中，我们使用正则表达式（`/\D/`）匹配所有的非数字字符，并用空字符串替换它们，因此"Hello, 123 world!456"变成了"123456"。

## 深入探究 (Deep Dive)

删除字符匹配模式是一种常见的文本处理技巧，它的历史跟编程语言及正则表达式的发展紧密相连。除了`preg_replace()`外，PHP还提供了很多其他的字符串处理函数。当你处理复杂的模式匹配时，你可能需要更深入地学习正则表达式。

值得注意的一点是`preg_replace()`在处理较大字符串或复杂的模式匹配时可能会消耗较大的系统内存。在设计编程方案时，这是需要考量的一方面。

## 参考链接 (See Also)

- PHP官方手册字符串处理函数列表：
  http://php.net/manual/en/ref.strings.php
- 正则表达式教程：
  https://www.runoob.com/regexp/regexp-tutorial.html
- PHP官方手册关于preg_replace的更多信息：
  http://php.net/manual/en/function.preg-replace.php