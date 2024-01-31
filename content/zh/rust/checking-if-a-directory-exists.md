---
title:                "检查目录是否存在"
date:                  2024-01-20T14:58:38.679646-07:00
simple_title:         "检查目录是否存在"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
检查目录是否存在是文件系统操作，确定特定路径的目录是否真实可访问。程序员这么做以避免错误，比如尝试访问一个不存在的目录，导致程序崩溃。

## How to: (怎么做：)
Rust使用`std::path::Path`和`std::fs`处理文件系统。下面的例子展示了如何检查目录是否存在。

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/some/directory/path");

    if path.exists() {
        println!("目录存在！");
    } else {
        println!("目录不存在。");
    }
}
```

如果`/some/directory/path`存在，输出将是：

```
目录存在！
```

如果它不存在，输出将是：

```
目录不存在。
```

## Deep Dive (深入了解)
在文件系统中，判断一个位置是不是目录，是常见需求。早年，你可能需要使用操作系统的命令行工具或是低级语言直接与操作系统API对话。现在，高级语言例如Rust简化了过程。

备选方案包括使用Rust的`std::fs::metadata()`函数，这可以给你更多信息，比如文件类型和权限。用法如下：

```Rust
use std::fs;

fn main() {
    let path = "/some/directory/path";
    match fs::metadata(path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("目录存在！");
            } else {
                println!("路径存在，但不是目录。");
            }
        }
        Err(_) => println!("路径不存在。"),
    }
}
```

`std::path::Path`的`exists`方法底层实际上也是调用`std::fs::metadata`，但是它只关心路径是否存在，不关心是文件还是目录。

## See Also (另请参阅)
- Rust官方文件系统文档: [std::fs](https://doc.rust-lang.org/std/fs/)
- Rust路径处理相关文档: [std::path](https://doc.rust-lang.org/std/path/)
- Error处理相关教程: [Rust by Example - Error Handling](https://doc.rust-lang.org/rust-by-example/error.html)
