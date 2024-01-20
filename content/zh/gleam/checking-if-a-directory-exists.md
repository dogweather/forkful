---
title:                "检查目录是否存在"
date:                  2024-01-20T14:56:08.391128-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
检查目录是否存在就是确认你的程序或命令指向一个真实的文件夹。程序员这样做以避免错误，例如：尝试访问一个不存在的目录时导致程序崩溃。

## How to: (如何操作：)
在Gleam中，我们可以使用`std::fs`模块来检查一个目录是否存在。如果你熟悉Rust，这会觉得很熟悉。

```gleam
import gleam/erlang
import gleam/io

fn check_directory_exists(path: String) -> Bool {
  erlang.is_directory(path)
}

pub fn main() {
  let path = "path/to/directory"
  io.debug(check_directory_exists(path))
}
```

运行这个程序，如果目录存在，你会在控制台看到`True`，否则看到`False`。

## Deep Dive (深入了解)
检查目录是否存在的功能在程序语言的早期就出现了，因为这是文件系统交互的基础功能。在Gleam中，这个功能背后用的是Erlang的能力，因此性能和稳定性都相当可靠。除了`is_directory`直接检查外，你可以列出目录内容，然后判断是否为空，或者尝试创建或删除目录作为存在性的检测，但这些方法都不如直接检查来得直接和高效。

## See Also (另请参阅)
- Erlang's documentation on file operations which Gleam relies on: [https://erlang.org/doc/man/file.html](https://erlang.org/doc/man/file.html)
- A practical guide on file system operations in Erlang, relevant for understanding the underlying mechanisms that Gleam utilizes: [Learn You Some Erlang for Great Good - File I/O](https://learnyousomeerlang.com/file-io)