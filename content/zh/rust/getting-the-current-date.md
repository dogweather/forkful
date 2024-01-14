---
title:    "Rust: 获取当前日期"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么会获取当前日期？

在编程中，经常需要使用当前的日期来做记录、计算或者展示给用户。Rust提供了方便的方式来获取当前日期，让我们来学习一下！

## 如何获取当前日期？

要获取当前日期，我们需要使用`chrono`这个Rust的日期和时间处理库。首先，在你的Rust项目的`Cargo.toml`文件中，添加如下代码：

```Rust
[dependencies]
chrono = "0.4.0"
```

然后，在你的项目的代码中，导入`chrono`库：

```Rust
extern crate chrono;
use chrono::{Local, Datelike};
```

现在，我们就可以使用`chrono`来获取当前日期并对其进行处理。下面是一个例子，它会输出当前日期并显示是星期几：

```Rust
let current_date = Local::now();
println!("今天是{}月{}日，星期{}", current_date.month(), current_date.day(), current_date.weekday());
```

运行以上代码，你会得到如下输出：

```
今天是6月11日，星期四
```

## 深入了解获取当前日期

在`chrono`库中，`Local`类型代表了当前系统的本地时间。当我们使用`Local::now()`来获取当前日期时，它实际上是一个`DateTime<Local>`类型的值，其中包含了具体的年、月、日、时、分、秒等信息。通过调用相应的方法，我们可以从`DateTime`类型中提取这些信息。

另外，`chrono`库还提供了方便的日期格式化功能，让日期可以以不同的样式显示给用户。例如，我们可以使用`format`方法来自定义日期的格式。下面是一个例子，它会以YYYY-MM-DD的形式显示当前日期：

```Rust
let current_date = Local::now();
println!("{}", current_date.format("%Y-%m-%d"));
```

运行以上代码，你会得到如下输出：

```
2020-06-11
```

# 查看更多

- chrono文档：https://docs.rs/chrono/0.4.0/chrono/
- Rust官方文档：https://www.rust-lang.org/zh-CN/
- Rust中文社区论坛：https://rust.cc/