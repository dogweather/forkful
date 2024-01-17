---
title:                "将字符串大写"
html_title:           "Elm: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 是什么 & 为什么？
对一个字符串进行 capitalization 是指将字符串中的首字母变为大写。程序员这样做的原因是为了让字符串更容易阅读，以及符合特定的代码规范。

## 如何：
```Elm
-- 例1：将字符串 "hello world" 进行 capitalization
String.toUpper "hello world"

-- 输出： "Hello world"
```
```Elm
-- 例2：将字符串 "elm programming" 进行 capitalization
String.toUpper "elm programming"

-- 输出： "Elm programming"
```

## 深入探讨：
- 历史背景：在早期的程序设计中，大部分程序语言都采用全大写的方式来书写代码和变量名。但是随着编程语言的发展，capitalization 逐渐成为一种编程风格和规范。
- 替代方法：除了 Elm 中的 `String.toUpper` 函数，也可以使用 `String.toTitle` 函数来进行 capitalization。不过要注意，`toTitle` 函数不仅会将首字母变为大写，还会将字符串中每个单词的首字母都变为大写。
- 实现细节：在 Elm 中，使用 `String.toUpper` 函数实现 capitalization 的原理是先将字符串转换为单个字符的列表，再对列表中的首字母进行大写转换，最后再将列表转换为字符串。

## 参考资料：
- Elm 的官方文档：https://guide.elm-lang.org/
- `String.toUpper` 函数的详细说明：https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- `String.toTitle` 函数的详细说明：https://package.elm-lang.org/packages/elm/core/latest/String#toTitle