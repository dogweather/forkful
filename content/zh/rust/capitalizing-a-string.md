---
title:                "将字符串大写"
html_title:           "Rust: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何为 & 为何？ (What & Why?)

将字符串大写，就是把字符串中的所有小写字母转化成大写字母。 这对于增强显眼度、格式统一或者识别性能优化非常有用。

## 如何: (How to)

```Rust
let s = "hello";
let capitalized_s = s.to_uppercase();
println!("{}", capitalized_s);
// Output: HELLO
```

## 深度了解 (Deep Dive)

- 历史背景：Rust语言保持了大写转化的传统，对 Unicode 字符串也能正确处理，这比早期编程语言更虹霓。

- 变体：将字符串首字母大写。这在某些情况下，比如处理人名时，常常使用。
  ```Rust
  let s = "rust programming";
  let capitalized_s = s.chars().enumerate().map(|(i, c)| if i == 0 { c.to_uppercase().to_string() } else { c.to_string() } ).collect::<String>();
  println!("{}", capitalized_s);
  // Output: Rust programming
  ```

- 实现细节：Rust语言通过标准库函数`to_uppercase`来大写字符串。这个函数对于Unicode字符串兼容性良好，不仅可以处理ASCII字符，还可以处理其他任何Unicode字符。

## 查看更多 (See Also)

- [官方字串大写文档](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [字符串和Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [如何在 Rust 中处理 Unicode](https://www.joshmcguigan.com/blog/unicode-chars-rust/)