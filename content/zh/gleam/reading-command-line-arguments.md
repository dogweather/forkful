---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么与为什么？

命令行参数是由用户在启动程序时传入的外部输入。程序员使用它来控制程序的行为，提供更大的自由度和灵活性。

## 如何？

我们可以使用 Gleam 的 `os.args()` 函数来读取命令行参数，返回一个参数字符串列表。

计算机：
```Gleam
import gleam/os.{args}

fn main() {
  let arguments = os.args()
  arguments
  |> list.map(fn(a) { io.println(a) })
  |> Ok(Nil)
}
```
然后，执即可行该程序并传入任何你想要的参数。比如同时传入 "hello" 和 "world"：
```bash
$ gleam run main hello world
hello
world
```

## 深入解析

Gleam 在命令行环境下提供了一种简洁的方法读取参数，这是 Unix 风格的 CLI 设计中的一部分，已经有几十年的历史了。而大多数其他编程语言都提供了类似的机制。

加入你需要更复杂的参数解析，你可能需要使用预先构建的库，例如 `getopt`。

Gleam 的基础实现中，包含了调用 Erlang 运行时系统（ERTS）底层的 `init:get_plain_arguments/0` 函数，该函数在程序启动时获取命令行参数。

## 参考资料

- Gleam 主页: https://gleam.run 
- Gleam 文档: https://gleam.run/docs/
- Erlang init 文档: http://erlang.org/doc/man/init.html
- getopt 库: http://erlang.org/doc/man/getopt.html