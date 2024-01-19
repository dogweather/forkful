---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？
创建临时文件是一种常见的编程任务，用于存储数据，该数据在程序运行期间可能会更改，或者我们不希望保存。 程序员创建临时文件以便于测试、调试以及某些数据处理任务。

## 怎么做：
Gleam 的标准库尚未包含具有创建临时文件功能的包，但我们可以通过 Erlang FFI 很容易实现这种功能。以下是一个例子：

```gleam
import gleam/ffi

fn mkstemp(prefix) {
  ffi.call("erlang", "mkstemp", [<<prefix>>])
}

pub fn temp_file() {
   mkstemp("temp_")
}
```
运行这段代码会输出与以 "temp_" 为前缀的临时文件名有关的信息。

## 深入挖掘
在过去，程序员创建临时文件是为了存储大量数据，这些数据太大，无法放在内存中。 随着 RAM 价格的降低，这种情况已经不太常见，但是还有许多其他原因可能需要创建临时文件。

另一个选择是使用内存文件系统，这意味着文件实际上存储在 RAM 中 而不是磁盘上，这可能更快，但是也有内存限制。

上述 `mkstemp()` 函数的实现依赖 Erlang 的 `mkstemp` 函数，它创建了一个具有唯一名称的安全临时文件，并返回新创建的文件路径。

## 另请参阅
- Erlang 'mkstemp': http://erlang.org/doc/man/file.html#mkstemp-1
- Gleam FFI: https://gleam.run/book/tour/ffi.html
- Gleam 入门教程: https://gleam.run/getting-started/