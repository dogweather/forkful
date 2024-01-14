---
title:    "Rust: 检查目录是否存在"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么：为什么要学习如何检查目录是否存在？因为在编程中常常需要在程序执行前先检查目录是否存在，以防止出现错误或者程序崩溃。这是一种增加代码健壮性的常用方法。

如何：Rust是一种现代化的编程语言，它提供了许多不同的方法来检查和处理目录。下面是一个简单的示例代码，展示如何使用Rust中的标准库来检查目录是否存在。

```Rust
use std::fs;
use std::io;

fn main() {
    let directory = "my_folder";

    // 使用fs::metadata函数来获取目录的元数据
    let directory_metadata = fs::metadata(directory);

    // 使用Result类型来处理可能出现的错误
    match directory_metadata {
        Ok(metadata) => {
            // 如果目录存在，则打印出目录的路径和大小
            if metadata.is_dir() {
                println!("目录 {} 存在，大小为 {} bytes.", directory, metadata.len());
            } 
        }
        Err(error) => {
            // 如果出现错误，打印错误信息
            println!("无法访问目录 {}: {}", directory, error);
        }
    }
}

```

运行上面的代码，如果目录“my_folder”存在，将会输出如下信息：

```
目录 my_folder 存在，大小为 4096 bytes.
```

深入了解：实际上，fs::metadata函数不仅可以用来检查目录是否存在，还可以用来获取目录的元数据，如目录的权限、所属用户等信息。此外，Rust还提供了其他函数来检查目录的存在性，例如fs::read_dir和fs::create_dir，可以根据自己的需要选择使用。

也值得一提的是，Rust中的标准库还提供了一些宏来简化目录检查和操作的代码，例如`create_dir`宏可以用于创建目录，`canonicalize`宏可以用来将相对路径转换为绝对路径。

参考链接：

- [Rust标准库文档 - fs模块](https://doc.rust-lang.org/std/fs/index.html)
- [Rust By Example - 文件/目录操作](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)

请进一步探索Rust的标准库，了解更多关于目录操作的方法和技巧吧。

## 参考链接

- [Rust标准库文档 - fs模块](https://doc.rust-lang.org/std/fs/index.html)
- [Rust By Example - 文件/目录操作](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)