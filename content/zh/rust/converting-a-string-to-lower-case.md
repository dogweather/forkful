---
title:    "Rust: 将字符串转换为小写"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么要使用Rust将字符串转换为小写

在Rust编程中，字符串是一个常见的数据类型，经常需要对其进行一些处理。有时候，我们可能需要将字符串的所有字符转换为小写，以方便后续的比较或处理。这时候，使用Rust提供的转换方法可以简化我们的编程工作。

## 如何实现

Rust中有两种方法可以实现将字符串转换为小写：一种是直接使用标准库提供的方法，另一种是自己编写转换函数。下面分别介绍这两种方法的具体实现及输出结果。

### 使用标准库方法

Rust标准库中提供了`.to_lowercase()`方法，可以将字符串转换为小写。下面是一个简单的例子：

```Rust
let my_str = "Hello World!";
let lower_str = my_str.to_lowercase();
println!("{}", lower_str);
```

输出结果为：

```bash
hello world!
```

在这个例子中，我们首先定义一个字符串变量`my_str`，然后使用`.to_lowercase()`方法将其转换为小写，并将转换后的值赋给新的变量`lower_str`。最后，使用`println!`宏打印出转换后的结果。

### 自己实现转换函数

除了使用标准库提供的方法，我们也可以自己编写转换函数来实现将字符串转换为小写。下面是一个简单的示例：

```Rust
fn to_lowercase(my_str: &str) -> String {
    let mut result = String::new();

    for c in my_str.chars() {
        if c.is_uppercase() {
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }

    result
}

fn main() {
    let my_str = "Hello World!";
    let lower_str = to_lowercase(my_str);
    println!("{}", lower_str);
}
```

在这个例子中，我们首先定义了一个`to_lowercase()`函数，接受一个字符串参数，并返回转换后的小写字符串。然后，在`main()`函数中调用这个自定义函数，并打印出转换后的结果。

## 深入了解

无论是使用标准库的方法还是自己实现转换函数，本质上都是通过遍历字符串中的每个字符，对每个字符进行小写转换。不过，使用自己的转换函数可以更加灵活，比如可以根据实际需求，加入一些自定义的逻辑来实现转换。另外，也可以尝试使用Rust提供的其他方法来实现字符串转换，从而提升代码的性能和效率。

## 参考资料

- Rust官方文档：https://doc.rust-lang.org/
- Rust标准库API文档：https://doc.rust-lang.org/std/
- Rust编程语言: https://www.rust-lang.org/zh-CN/
- Rust中文社区：https://rust.cc/

## 参考阅读

- [How do I convert string to lowercase in Rust?](https://stackoverflow.com/questions/27293169/how-do-i-convert-string-to-lowercase-in-rust)
- [字符串操作](https://rust-lang.org/zh-CN/book/ch08-02-strings.html#%E5%86%85%E7%BD%AE%E6%93%8D%E4%BD%9C%E7%B1%BB%E5%9E%8B%E6%96%B9%E6%B3%95)
- [Rust中文指南：标准库](https://rust.lang-cn.org/standardlibrary/index.html)