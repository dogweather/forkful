---
title:                "检查目录是否存在"
html_title:           "Rust: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

有时候在编程过程中，我们可能需要检查某个目录是否存在。这可以帮助我们避免不必要的错误，同时也可以让我们更加动态地处理文件操作。

## 怎么做

```Rust
use std::fs;

fn main() {
    let path = "example_folder"; // 假设需要检查的目录为 "example_folder"

    // 使用 fs::metadata 函数来获取目录信息
    match fs::metadata(path) {
        Ok(metadata) => {
            // 如果检查成功，则打印出 "example_folder 存在"
            println!("{} 存在", path);
        }
        Err(_) => {
            // 如果检查失败，则打印出 "example_folder 不存在"
            println!("{} 不存在", path);
        }
    }
}
```

运行以上代码可以得到类似下面的输出：

```text
example_folder 存在
```

如果目录不存在，则会得到以下输出：

```text
example_folder 不存在
```

## 深入了解

在 Rust 中，我们可以使用 `std::fs` 库来处理文件和目录相关的操作。其中，`metadata` 函数可以用来获取给定路径的元数据，包括文件类型、大小、修改时间等信息。如果给定的路径不存在，该函数会返回一个错误。

要注意的是，目录不存在并不一定意味着出错。例如，我们可以通过 `std::fs::create_dir` 函数来创建该目录后再进行检查。

## 参考链接

- [std::fs - Rust标准库文档](https://doc.rust-lang.org/std/fs/index.html)
- [Rust入门教程 - 目录](https://kaisery.github.io/trpl-zh-cn/ch12-00-an-io-project.html#%E7%A1%AC%E7%9B%98)
- [RustByExample - 目录操作](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)