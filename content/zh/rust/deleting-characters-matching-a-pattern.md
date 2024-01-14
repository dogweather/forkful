---
title:                "Rust: 删除匹配模式的字符"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

为什么：为什么有人会执行删除匹配模式的字符？

有时候在编程中，我们需要对字符串中特定的字符进行删除，比如删除所有的空格或者特定的符号。这样可以让我们的数据更加干净和规范。

## 怎么做

在Rust中，我们可以使用标准库中的字符串操作函数来删除特定的字符。下面是一个简单的例子：

```Rust
fn main() {
  // 创建一个字符串
  let mut s = String::from("I ❤ Rust 💻");
  
  // 删除所有的 ❤ 符号
  s.retain(|c| c != '❤');
  
  // 输出结果
  println!("{}", s);
}
```

运行结果为：

```
I  Rust 💻
```

这里我们使用了 `retain()` 函数，它接受一个闭包作为参数，用于判断每个字符是否要保留。在这个例子中，我们使用不等于 `!=` 来判断是否为我们要删除的 `❤` 符号。

## 深入了解

除了使用 `retain()` 函数，我们还可以使用 `replace()` 函数来将匹配的字符替换为其他字符。这里有一个使用正则表达式来删除所有数字的例子：

```Rust
use regex::Regex;

fn main() {
  // 创建一个字符串
  let mut s = String::from("Hello 123 Rust!");
  
  // 创建一个正则表达式
  let re = Regex::new(r"\d").unwrap();
  
  // 使用 replace() 函数替换所有数字为空字符
  s = re.replace_all(&s, "").to_string();
  
  // 输出结果
  println!("{}", s);
}
```

运行结果为：

```
Hello  Rust!
```

在深入了解如何使用字符串操作函数来删除字符之前，建议先熟悉Rust中的字符串类型和标准库中的字符串操作函数。同时，掌握正则表达式也是非常有帮助的。

## 参考链接

- [Rust官方文档 - 字符串类型](https://doc.rust-lang.org/std/string/index.html)
- [Rust官方文档 - 标准库中的字符串操作函数](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [正则表达式基础教程](https://deerchao.net/tutorials/regex/regex.htm)