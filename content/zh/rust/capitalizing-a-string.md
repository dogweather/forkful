---
title:    "Rust: 字符串大写化"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么
Rust是一种现代的，强大的编程语言，被越来越多的开发者所青睐。它的最大优点之一就是其丰富的标准库，提供了许多方便的函数和方法来处理数据。其中之一就是 `to_uppercase()`函数，它可以将一个字符串中的所有小写字母转换为大写字母。那么为什么我们需要这个函数呢？有时候，我们需要在我们的程序中对用户的输入做一些处理，比如用户输入的用户名可能包含大小写混合的情况，但是在后续处理中，我们需要保证所有的用户名都是大写的。这时候， `to_uppercase()`函数就能派上用场了。

## 如何操作
要使用 `to_uppercase()`函数，首先需要在 `std`标准库中引入 `String`类型。然后，使用点操作符来调用该函数，如下所示：

```Rust
use std::string::Stirng;

let username = String::from("rustfan");

let uppercase_username = username.to_uppercase();

println!("Uppercase username: {}", uppercase_username);
```

这里我们使用 `String`类型的 `to_uppercase()`函数将 `username`转换为大写并将其保存到一个新的变量中。最后，使用 `println!`宏来打印转换后的结果。

## 深入探讨
实际上，由于Rust的所有权系统，我们不能直接对字符串进行修改而是需要创建一个新的字符串。这也就意味着，每次调用 `to_uppercase()`函数时都会进行一次内存分配，这在性能上可能会有一些影响。因此，在处理大量字符串时，我们可以考虑使用 `to_ascii_uppercase()`函数，它将字符串中的所有字符转换为大写字符，但不会进行内存分配。

## 看看这些
- [Rust文档：标准库](https://doc.rust-lang.org/std/)
- [Rust文档：String类型](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust By Example：Strings](https://doc.rust-lang.org/stable/rust-by-example/std/strings.html)