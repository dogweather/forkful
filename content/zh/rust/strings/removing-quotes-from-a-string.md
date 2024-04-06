---
date: 2024-01-26 03:41:44.171026-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5F53\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\
  \u9664\u5F15\u53F7\u65F6\uFF0C\u4F60\u53EF\u80FD\u4F1A\u60F3\u77E5\u9053\u4E3A\u4EC0\
  \u4E48\u4E0D\u53EA\u662F\u7B80\u5355\u5730\u4F7F\u7528 `.replace(\"\\\"\", \"\"\
  )`\u3002\u65E9\u671F\uFF0C\u5904\u7406\u6587\u672C\u7684\u6807\u51C6\u5316\u8F83\
  \u5C11\uFF0C\u4E0D\u540C\u7CFB\u7EDF\u6709\u5B58\u50A8\u548C\u4F20\u8F93\u6587\u672C\
  \u7684\u4E0D\u540C\u65B9\u5F0F\uFF0C\u901A\u5E38\u5BF9\u7279\u6B8A\u5B57\u7B26\u4F7F\
  \u7528\u67D0\u79CD\u201C\u8F6C\u4E49\u5E8F\u5217\u201D\u3002Rust \u7684 `trim_matches`\u2026"
lastmod: '2024-04-05T22:51:00.711559-06:00'
model: gpt-4-0125-preview
summary: "\u5F53\u7136\u4E5F\u6709\u66FF\u4EE3\u65B9\u6CD5\u3002Regex \u662F\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\u7684\u5F3A\u5927\u5DE5\u5177\uFF0C\u80FD\u591F\u5339\u914D\
  \u590D\u6742\u7684\u6A21\u5F0F\uFF0C\u800C\u4E14\u5BF9\u4E8E\u4EC5\u4EC5\u79FB\u9664\
  \u5F15\u53F7\u6765\u8BF4\u53EF\u80FD\u6709\u70B9\u5927\u6750\u5C0F\u7528\u3002\u50CF\
  \ `trim_in_place` \u8FD9\u6837\u7684\u5E93\u80FD\u63D0\u4F9B\u539F\u5730\u4FEE\u526A\
  \u800C\u4E0D\u9700\u8981\u521B\u5EFA\u65B0\u7684 `String` \u5BF9\u8C61\uFF0C\u8FD9\
  \u5BF9\u4E8E\u6027\u80FD\u5173\u952E\u7684\u5E94\u7528\u6765\u8BF4\u53EF\u80FD\u662F\
  \u53EF\u53D6\u7684\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作：
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"你好，Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // 输出：你好，Rustaceans!
}
```

有时你可能会碰到一个含有混合引号的字符串，像这样：

```Rust
fn main() {
    let mixed_quoted = "'Rust说：\"你好，世界!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // 输出：Rust说："你好，世界!"
}
```

在这里，只有最外层的单引号被移除了。

## 深入解析
当从字符串中移除引号时，你可能会想知道为什么不只是简单地使用 `.replace("\"", "")`。早期，处理文本的标准化较少，不同系统有存储和传输文本的不同方式，通常对特殊字符使用某种“转义序列”。Rust 的 `trim_matches` 方法更加多功能，允许你指定多个要修剪的字符，以及是从字符串的开始（前缀）、结束（后缀）还是两侧进行修剪。

当然也有替代方法。Regex 是字符串操作的强大工具，能够匹配复杂的模式，而且对于仅仅移除引号来说可能有点大材小用。像 `trim_in_place` 这样的库能提供原地修剪而不需要创建新的 `String` 对象，这对于性能关键的应用来说可能是可取的。

在底层，`trim_matches` 实际上是从字符串的两端开始，通过字符地迭代检查所提供的模式，直到找到不匹配的字符。对于它所做的工作而言，它是高效的，但总是要意识到它是在处理 Unicode 标量值。如果你的字符串可能包含多字节的 Unicode 字符，你不必担心它会将它们分开。

## 参见
- Rust 关于字符串操作的文档：https://doc.rust-lang.org/book/ch08-02-strings.html
- 用于复杂模式的 `regex` 库：https://crates.io/crates/regex
- Rust 通过实例学习编程场景：https://doc.rust-lang.org/stable/rust-by-example/std/str.html
