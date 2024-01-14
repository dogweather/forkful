---
title:                "Rust: 将日期转换为字符串"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么
在编写程序时，经常会涉及到日期的操作。将日期转换成字符串是一个常见的需求，它可以让我们更方便地在程序中使用日期数据。对于使用Rust语言的开发者来说，掌握如何将日期转换成字符串是非常重要的。

# 怎么做
```Rust
// 导入日期相关的库
use chrono::{DateTime, TimeZone, Utc}; 
// 定义一个日期变量
let date = Utc.ymd(2021, 10, 1); 
// 将日期转换成字符串
let date_str = date.to_string(); 
// 输出结果
println!("日期转换成字符串结果为: {:?}", date_str); 
```
以上代码使用了Rust中的[Chrono库](https://crates.io/crates/chrono)，通过调用其中的函数和方法，我们可以轻松地将日期转换成字符串。在代码中，我们首先导入了`DateTime`、`TimeZone`和`Utc`这几个相关的结构体和trait。接着定义了一个日期变量，并使用`to_string()`方法将其转换成字符串。最后通过`println!()`宏输出转换后的结果。

运行以上代码，我们可以得到如下结果：
```
日期转换成字符串结果为: 2021-10-01 00:00:00 UTC
```
通过这个简单的例子，我们可以看到如何使用Rust中的库来实现日期转换成字符串的功能。当然，在实际开发中，可能会有更复杂的需求，下面让我们来深入探讨一下。

# 深入了解
在Rust中，日期的表示一般是通过`DateTime`结构体来实现的。该结构体包含了年、月、日、时、分、秒等信息。当我们调用`to_string()`方法将其转换成字符串时，实际上是将其按照一定的格式进行了格式化。默认情况下，输出的格式为`YYYY-MM-DD HH:MM:SS UTC`，其中`UTC`指的是协调世界时。当然，我们也可以通过修改相关的参数来自定义输出的格式，具体可以参考[官方文档](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html#method.format)。

另外，除了使用`to_string()`方法来将日期转换成字符串之外，我们也可以使用`format!()`宏来实现相同的功能。这两种方法的最大区别在于，`to_string()`方法是针对特定的数据类型而存在的，而`format!()`宏可以用来格式化任意类型的数据。因此，如果我们需要将日期以外的其他数据转换成字符串，可以选择使用`format!()`宏。

# 参考链接
- [Chrono库](https://crates.io/crates/chrono)
- [Rust官方文档](https://www.rust-lang.org/zh-CN)
- [Rust中文社区](https://rust-china.org)