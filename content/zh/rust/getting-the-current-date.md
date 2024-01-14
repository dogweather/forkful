---
title:    "Rust: 获取当前日期"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# 为什么要获取当前日期？
获取当前日期是编程中最常见的需求之一。无论是在网站中显示当前日期，还是在程序中进行时间计算，获取当前日期都是必不可少的操作。Rust语言提供了简单易用的方法来获取当前日期，让我们一起来学习吧！

## 如何获取当前日期
为了获取当前日期，我们需要使用Rust标准库中的`time`模块。首先，我们需要在代码中引入该模块：
```
use std::time;
```
然后，我们就可以使用`time::SystemTime`结构体的`now()`方法来获取当前日期。代码如下：
```
let now = time::SystemTime::now();
```
接下来，我们可以使用`time::SystemTime`结构体的`duration_since()`方法来计算当前日期与某一日期的时间间隔。例如，我们可以计算当前日期与1970年1月1日的时间间隔：
```
let since_epoch = now.duration_since(time::UNIX_EPOCH).expect("Error");
```
最后，我们使用`format()`方法将日期格式化为我们想要的形式，例如年月日的形式：
```
let date = since_epoch.as_secs();
let formatted_date = time::strftime("%Y-%m-%d", &time::at(time::Timespec::new(date as i64, 0))).unwrap();
println!("{}", formatted_date);
```
运行以上代码，我们就可以得到当前日期的年月日形式输出。

## 深入了解获取当前日期
Rust中的`time`模块内部实际上是使用C语言的`time`库来进行日期和时间的操作。因此，Rust中获取当前日期的方法与C语言中基本相同。

需要注意的是，Rust中获取到的当前日期是以从1970年1月1日以来经过的时间间隔来表示的，这也被称为Unix时间戳。在进行日期计算时，我们需要将其转换为我们熟悉的年月日的形式。

## 参考链接
- Rust官方文档：https://doc.rust-lang.org/std/time/
- 系统时间和日期：https://www.jianshu.com/p/749dc7e2e96e
- Rust标准库中的`time`模块：https://rustlang-cn.org/office/rust/src/core/time/

# 请参阅
- [深入理解Rust标准库中的时间模块](https://blog.csdn.net/qq_40877296/article/details/102666510)
- [使用Rust获取当前日期和时间](https://www.it1352.com/2216565.html)