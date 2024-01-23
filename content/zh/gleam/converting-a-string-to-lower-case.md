---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:17.971842-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
把字符串转换成小写，就是将所有的大写字符改成小写形式。程序员这么做是为了统一数据格式、简化比较和搜索。

## How to (如何操作)
用Gleam实现字符串转小写很直接。下面看代码和结果：

```gleam
import gleam/string

pub fn main() {
  let greeting = "Hello, World!"
  let lowercased_greeting = string.to_lower(greeting)
  lowercased_greeting
}
```

输出会是：

```
"hello, world!"
```

## Deep Dive (深入了解)
在编程语言的早期，大小写转换并不常见。随着搜索和排序的需求，这个功能变成标准。有些语言使用专门的方法（如Gleam的`string.to_lower`），有些则是字符串对象的自有函数。

大小写转换不只是对英文来说有意义，对于其他有大写和小写概念的语言（如德语或是土耳其语）也是。需要注意的是，大小写映射并不总是一对一的，在某些语言中，可能一个大写字母对应多个小写字母。因此，编程语言通常使用Unicode标准来处理大小写转换。

还有，如果不使用Gleam自带的功能，你可以手动转换字符。这涉及到字符编码和处理大小写之间的差异，但通常情况下，使用内建方法更方便、可靠。

## See Also (参考资料)
- Unicode 标准大小写映射表：[Unicode Case Mapping](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
- 关于 Unicode 和字符编码深入了解：[Unicode Standard](https://www.unicode.org/standard/standard.html)
