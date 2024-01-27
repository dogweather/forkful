---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
字符串大写，就是把所有字母变成大写字母。程序员这么做为了统一格式，提升可读性，或者满足编程需求。

## How to: 怎么做

```Rust
fn main() {
    let greeting = "hello, world!";
    println!("{}", greeting.to_uppercase());
}
```

输出：
```
HELLO, WORLD!
```

## Deep Dive 深入探索

字符串大写不是Rust特有的；其他编程语言也有类似功能。在 Rust 中，`.to_uppercase()` 方法会遍历字符串中的每个字符，将其转换为大写形式。如果字符没有大写等价形式，就会保持不变。这一过程遵循Unicode标准。

Rust提供的`.to_uppercase()`方法与其他语言不同之处在于它对Unicode字符有很好的支持。不像某些语言只能处理ASCII码，Rust可以正确大写所有语言的字母，包括带变音符的字母。

此外，还有其它方法可以改变字符串的大小写，比如`.make_ascii_uppercase()`，但这个方法只对ASCII字符有效，对Unicode字符无效。

## See Also 相关链接

- Rust 标准库中的 `.to_uppercase()` 方法文档: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Unicode标准: http://www.unicode.org/standard/standard.html
- Rust `char` 类型和它的方法，包括 `.to_uppercase()`: https://doc.rust-lang.org/std/primitive.char.html
- 更广泛的字符串处理方法讨论: https://rust-lang-nursery.github.io/rust-cookbook/strings.html
