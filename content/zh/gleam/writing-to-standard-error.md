---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么?

标准错误（stderr）是用来输出错误信息的。程序员这样做能分开正常输出和错误信息，方便调试和日志记录。

## How to 怎么做

```gleam
import gleam/io

fn main() {
  io.println("Logging to stdout")
  io.eprint("Oops! An error occurred.")
}
```

输出：
```
Logging to stdout
Oops! An error occurred.
```

## Deep Dive 深入探索

过去，区分stdout和stderr有助于信息的合适管道处理。替代方法包括使用日志框架或文件。Gleam通过`io`标准库来实现写入标准错误。

## See Also 更多信息

- Unix 哲学与标准流：[https://en.wikipedia.org/wiki/Unix_philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)
