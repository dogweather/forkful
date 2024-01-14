---
title:                "Rust: 打印调试输出"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

打印调试信息是编程中必不可少的一项技能。它可以帮助开发者理解程序运行中的细节，从而更容易地发现和解决bug。同时，它也能帮助开发者在代码中定位到特定的位置，提高调试效率。

## 如何做

在Rust中，打印调试信息的方法非常简单。我们可以使用宏`println!`来将信息输出到标准输出。下面是一个简单的例子：

```Rust
let num = 10;
println!("The value of num is {}", num);
```

输出结果为：`The value of num is 10`

我们可以在花括号内使用参数，这些参数会按顺序替换占位符`{}`，从而输出对应的值。同时，我们也可以使用多个占位符来输出多个值。

```Rust
let name = "Mandarin";
let age = 100;
println!("My name is {}, I am {} years old.", name, age);
```

输出结果为：`My name is Mandarin, I am 100 years old.`

除了使用`println!`宏外，我们也可以使用`eprintln!`宏将信息输出到标准错误。这在调试时非常有用，因为标准错误会以红色显示，更容易被注意到。

## 深入探究

在Rust中，我们可以通过在字符串前面添加`#`来使用类似`println!`宏的`format!`宏。这允许我们使用类似C语言中`printf`函数的格式化字符串来输出信息。

```Rust
let num = 1.23456;
println!("The formatted value is {:#.2}", num);
```

输出结果为：`The formatted value is 1.23`

我们也可以通过宏`dbg!`来打印变量的值及其类型。这在调试复杂数据结构时非常方便。

```Rust
let vec = vec![1, 2, 3];
println!("{:?}", vec);
```

输出结果为：`[1, 2, 3]`

## 参考链接

- Rust官方文档：[https://rust-lang.org/](https://rust-lang.org/)
- Rust编程语言：[https://www.rust-lang.org/](https://www.rust-lang.org/)
- Rust中文社区：[https://rust.cc/](https://rust.cc/)