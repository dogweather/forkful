---
title:                "创建临时文件"
html_title:           "Rust: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

创建临时文件是处理文件输入和输出的常用方式。它允许我们快速地在程序运行时创建临时文件来存储数据，而不会影响到我们原本的文件。

## 如何操作

创建临时文件可以使用标准库中的 `tempfile` 模块。首先，我们需要导入 `tempfile::NamedTempFile` 类。然后，使用 `create()` 方法来创建一个临时文件对象。最后，可以使用 `write_all()` 方法来向文件中写入数据。

```Rust
use tempfile::NamedTempFile;
let mut temp_file = NamedTempFile::create().unwrap();
temp_file.write_all(b"Hello, world!").unwrap();
```

如果需要获取临时文件的路径，可以使用 `path()` 方法。

```Rust
let temp_file_path = temp_file.path();
```

最后，记得在不再需要临时文件时，使用 `close()` 方法来关闭并删除它。

```Rust
temp_file.close().unwrap();
```

## 深入了解

创建临时文件时，我们可以指定文件的名称和文件所在的目录。如果没有指定，系统就会使用默认的临时目录来创建文件。可以使用 `new()` 方法来指定文件名和目录路径。

```Rust
use tempfile::Builder;
let temp_file = Builder::new()
    .prefix("prefix")
    .suffix(".txt")
    .tempfile()
    .unwrap();
```

除了创建临时文件外，`tempfile` 模块还提供了其他实用的功能，比如创建临时目录，将文件重命名等操作。想要更多了解，可以查阅[官方文档](https://docs.rs/tempfile/)。

## 参考链接

- [Rust编程语言](https://www.rust-lang.org/)
- [临时文件的创建和读写](https://rust-lang-nursery.github.io/rust-cookbook/file/temp.html)
- [Rust标准库文档-tempfile模块](https://doc.rust-lang.org/std/io/struct.Stdin.html#example)