---
title:                "字符串大写化"
html_title:           "Gleam: 字符串大写化"
simple_title:         "字符串大写化"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 什么？为什么？

将字符串的首字母大写是指将字符串中的第一个字母改为大写。程序员会这样做是为了让字符串更易读并且符合一致的命名规范。

## 如何：

```Gleam
fn capitalize(string) {
  let first_char = String.get(string, 0)
  let rest = String.slice(string, 1, String.length(string))
  let capitalized = String.capitalize(first_char) ++ rest
  capitalized
}
```

输入："hello world"

输出："Hello world"

## 深入探讨：

- 历史背景：大写字符串的习惯来自于传统的编程语言，如C和Java。
- 替代方案：还有其他方法可以实现大写字符串的功能，如正则表达式和循环遍历字符串。
- 实现细节：在Gleam中，使用内置的String模块提供的capitalize函数可以轻松实现字符串大写功能。

## 参考：

- [Gleam Programming Language Documentation](https://gleam.run)
- [Why do programmers capitalize strings?](https://stackoverflow.com/questions/58540287/why-do-programmers-capitalize-strings)
- [Java String Capitalize](https://www.javatpoint.com/java-string-capitalize)