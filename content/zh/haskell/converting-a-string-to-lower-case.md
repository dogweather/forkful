---
title:                "转换字符串为小写"
html_title:           "Haskell: 转换字符串为小写"
simple_title:         "转换字符串为小写"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是字符串转换为小写？为什么程序员要这么做？

字符串转换为小写是指将字符串中的所有字母都转换为小写。程序员通常会这么做是因为在编程中，字符串的大小写是敏感的，因此为了减少比较的复杂性，我们需要将字符串统一为同一种大小写形式。

## 如何操作：

```Haskell
import Data.Char (toLower)

-- 将字符串中的所有字母转换为小写
lowercaseString :: String -> String
lowercaseString str = map toLower str

-- 输出示例
lowercaseString "HELLO, WORLD!"  
-- "hello, world!" 
```

## 深入了解：
字符串转换为小写的思想源于计算机科学中的ASCII编码，ASCII编码规定了每个字符对应的十进制数值，大写字母和小写字母的数值相差32。因此，通过将字符的数值加上或减去32，就可以实现大小写的转换。除了使用```Data.Char```中的```toLower```函数外，我们还可以使用```ord```和```chr```函数来实现这种转换。

## 相关资料：
- [Haskell官方文档：Data.Char模块](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [ASCII编码表](http://www.asciitable.com/)