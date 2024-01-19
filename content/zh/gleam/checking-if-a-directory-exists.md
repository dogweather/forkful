---
title:                "检查目录是否存在"
html_title:           "Gleam: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 检查目录是否存在的Gleam编程
"## 什么以及为什么？
检查目录是否存在是一个步骤，在这个步骤中，编程语言会看看所给的路径指向的目录是否确实存在。这是为了保证我们的代码能在预期的地方找到或者创建文件，避免出现错误。

## 如何去做：
在Gleam中你可以使用 Erlang/Elixir 的 File 模块来检查一个目录是否存在。例如：  

```gleam
let does_exist = erlang.unsafe_apply(
  "File", "dir?", list.from_array(list.cons(from_string("your_directory"), [])))
```
执行结果：
```gleam
Output: Ok(True)
```
当目录存在时，你会得到 `Ok(True)` 的输出，反之则是 ` Ok(False)`。

## 深度探索

历史背景：在早期的编程中，目录和文件管理是个复杂且耗时的任务。现在许多语言已经内建了这些功能。

替代方案：除了手动检查目录是否存在，你还可以尝试直接创建目录，如果目录已存在，则返回一个错误。

实现详情：这个检查操作使用了底层的操作系统函数，实际的执行细节可能依不同的操作系统有所不同。

## 更多阅读
- Gleam语言官方教程：[https://gleam.run/book/](https://gleam.run/book/)
- Erlang File模块的文档：[http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Gleam与Erlang / Elixir互操作性教程：[https://gleam.run/news/gleam-v0.14-released/](https://gleam.run/news/gleam-v0.14-released/)