---
title:                "插值字符串"
html_title:           "Rust: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

什么是插值字符串？

插值字符串是一种程序员使用的技巧，它可以允许我们将变量的值动态地嵌入到字符串中。这使得我们可以更容易地构建动态的和可定制的字符串，而不必手动拼接不同的变量和字符串。

为什么要插值字符串？

在编程中，我们经常需要动态地构建字符串，特别是在处理用户输入或从外部源获取数据时。插值字符串可以帮助我们更有效地构建这样的字符串，提高代码的可读性和可维护性。

如何实现插值字符串？

在Rust中，我们可以使用“{}”占位符将变量嵌入到字符串中。让我们来看一个简单的示例：

```Rust
let name = "Anna";
let age = 25;
let sentence = format!("我叫{}，今年{}岁。", name, age);
println!("{}", sentence);
```

输出：

我叫Anna，今年25岁。

让我们再看一个复杂一点的例子，使用条件语句和循环结合插值字符串：

```Rust
let numbers = [1, 2, 3, 4, 5];
let mut sum = 0;

for num in numbers.iter() {
  if num % 2 == 0 {
    println!("{}是偶数。", num);
  } else {
    println!("{}是奇数。", num);
  }

  sum += num;
}

println!("所有数字的总和是{}。",  sum);
```

输出：

1是奇数。
2是偶数。
3是奇数。
4是偶数。
5是奇数。
所有数字的总和是15。

深入了解

插值字符串的概念最早来自于Lisp语言中的“字符串插值符”，而后被广泛应用于其他编程语言中。除了使用“{}”占位符，一些语言还提供了更多的操作符和功能来帮助构建动态字符串。

除了插值字符串，还有其他的方法来构建动态字符串，比如字符串连接或格式化字符串。但插值字符串通常被认为是最简洁和最易于理解的方法，特别是当需要嵌入多个变量时。

如果你对插值字符串的实现机制感兴趣，你可以查看Rust中的“字符串格式化”文档。这可以帮助你更深入地了解如何实现插值字符串。

相关资源

- Rust字符串格式化文档：https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html
- 插值字符串的历史背景：https://en.wikipedia.org/wiki/String_interpolation
- 其他编程语言中的插值字符串实现：https://www.codewars.com/kata/51f2b4448cadf20ed0000389?utm_source=newsletter