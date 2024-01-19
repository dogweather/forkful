---
title:                "将字符串大写"
html_title:           "PHP: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 是什么?什么用? （What & Why?）

**大写化字符串**即把字符串中的小写字母转换成大写字母。程序员之所以这样做，主要是为了提升用户体验，比如美化显示文本，或者使值的比较变得不区分大小写。

## 怎么做? (How to:)

PHP中，我们有`strtoupper()`函数来进行字符串大写化。例子如下：

```PHP
<?php 
    $string = "hello world!";
    echo strtoupper($string); 
?>
```

它的输出结果 will 是：

```
HELLO WORLD!
```

只需一行代码，我们就能把所有小写 letters 转换成大写的了！

## 深挖一下： (Deep Dive)

有一些历史背景和更深入的信息可供参考：

1. **历史背景：** 在 ASCII 编码中，小写字母 'a' 到 'z' 对应的 ASCII 值比它们对应的大写字母 'A' 到 'Z' 大 32。这就是为什么 PHP 使用 ASCII 函数 `strtoupper()` 来大写化字符串的原理。

2. **变通方法：** 如果你不想对一个整个字符串进行大写化，PHP 还有其他函数可以实现。你可以使用 `ucfirst()` 函数只大写化字符串的首个字符，或者使用 `ucwords()` 函数来大写化每个单词的首字母。

3. **实现细节：** `strtoupper()` 函数不仅支持英文字符，也支持一些特定的非英文字符。但是，为了保证准确的结果，确保你的代码文件使用正确的字符编码是非常重要的。

## 更多相关： (See Also)

想深入了解 PHP 中的字符串处理和相关函数，你可以访问：
- [PHP: Strings - Manual](https://www.php.net/manual/en/language.types.string.php)
- [PHP: strtoupper - Manual](https://www.php.net/manual/en/function.strtoupper.php)

这两篇文章都是对 PHP 中字符串处理的很好的资源。