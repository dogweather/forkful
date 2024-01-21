---
title:                "创建临时文件"
date:                  2024-01-20T17:40:06.969173-07:00
model:                 gpt-4-1106-preview
simple_title:         "创建临时文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
什么 & 为什么？

创建临时文件就是在计算机上生成一个暂时存在的文件，通常用来存储过程数据或作为数据交换的桥梁。程序员这么做是因为有时候不需要或不想将文件内容永久保存在磁盘上。

## How to:
怎么做：

```Gleam
import gleam/io

pub fn create_temp_file() {
  let result = io.tmp_file()
  case result {
    Ok(file) -> file
    Error(err) -> 
      io.print("Failed to create a temporary file: ")
      io.print(err)
  }
}
```

执行上述代码应该不会有输出，因为目的只是生成文件而已。如果失败了，你会看到类似这样的错误信息：

```
Failed to create a temporary file: [错误详情]
```

## Deep Dive
深入了解：

在过去，程序员经常直接在系统的临时文件夹中手动创建临时文件，但这样有安全风险和数据竞态等问题。Gleam通过`io.tmp_file`函数提供了安全的创建临时文件的方式，它会处理所有的底层细节，比如为不同的文件生成唯一的名称以避免冲突。

此外，除了Gleam的`io.tmp_file`，程序员也可以使用标准库以外的包，或者直接调用操作系统的API来创建临时文件。不过这样通常需要更多的工作和处理更多的边边角角的事情。

## See Also
另请参阅：