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

## 什么 & 为什么？

检查目录是否存在是指在编程中判断一个指定的目录是否存在。程序员经常会这么做是因为当他们需要创建或访问特定的文件时，必须先确保目录存在，否则可能出现错误。


## 如何：

在Rust中，我们可以使用 ```std::fs::metadata``` 来检查目录的元数据，然后根据返回的结果判断目录是否存在。下面是一个例子：

```
use std::fs;

match fs::metadata("my_directory") {
    Ok(metadata) => {
        if metadata.is_dir() {
            println!("该目录存在！");
        } else {
            println!("该目录不存在！");
        }
    },
    Err(_) => println!("该目录不存在！"),
}
```

如果目录存在，上面的代码将输出 "该目录存在"，否则输出 "该目录不存在"。


## 深入了解：

在Rust之前的编程语言中，通常使用特定的系统调用来检查目录是否存在，例如C语言中的 ```opendir``` 函数。但是，Rust中提供了更加方便的方式，通过标准库的 ```std::fs``` 模块来处理文件系统。

除了使用 ```fs::metadata``` 外，我们也可以使用 ```fs::canonicalize``` 函数来返回一个 `Result<PathBuf>`，可以用来获取目录的绝对路径。如果目录不存在，这个函数也会返回一个错误。

你也可以使用其他第三方库来完成目录检查，例如 ```walkdir``` 或者 ```file```。

## 参考链接：

- Rust标准库文档：https://doc.rust-lang.org/std/fs/fn.metadata.html
- Rust中的文件系统操作：https://doc.rust-lang.org/book/ch01-03-hello-cargo.html
- ```walkdir```库：https://crates.io/crates/walkdir
- ```file```库：https://crates.io/crates/file