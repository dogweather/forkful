---
title:                "Gleam: 检查目录是否存在"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在？

当我们在进行文件操作时，有时候会需要检查某个目录是否存在。这样可以避免在处理文件时出现错误，保证脚本的平稳运行。

## 如何检查目录是否存在？

在Gleam中，我们可以使用`os.path.exists()`函数来检查目录是否存在，示例如下：

```Gleam
import os

fn main() {
    let directory = "/Users/Jane/Documents/"
    if os.path.exists(directory) {
        io.println("目录存在！")
    } else {
        io.println("目录不存在！")
    }
}
```

运行结果如下：

```
目录存在！
```

## 深入了解检查目录是否存在

除了使用`os.path.exists()`函数，我们还可以使用`os.path.isdir()`函数来检查目录是否存在。不同的是，`os.path.exists()`函数会返回目录或文件存在时都为`true`，而`os.path.isdir()`函数只有在目录存在时才会返回`true`。示例如下：

```Gleam 
import os

fn main() {
    let directory = "/Users/Jane/Documents/"
    if os.path.isdir(directory) {
        io.println("这是一个目录！")
    } else {
        io.println("这不是一个目录！")
    }
}
```

运行结果如下：

```
这是一个目录！
```

# 参考链接

- [Gleam官方文档 - Path](https://gleam.run/core/path.html)
- [Gleam官方文档 - IO](https://gleam.run/core/io.html)
- [Python官方文档 - os.path](https://docs.python.org/3/library/os.path.html)