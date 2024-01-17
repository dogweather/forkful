---
title:                "编写文本文件"
html_title:           "Elixir: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件？为什么要写它？

文本文件是一个存储纯文本数据的文件，它可以被计算机读取和处理。程序员通常会使用文本文件来存储和传输数据，比如配置文件、日志文件和代码文件。这样可以确保数据的可读性和一致性，方便进行后续的处理和编辑。

## 如何编写文本文件？

```Elixir
# 创建一个名为 "data.txt" 的文本文件，并写入 "Hello World!" 字符串
File.write("data.txt", "Hello World!")
```

```Elixir
# 追加数据到已存在的文本文件中
File.append("data.txt", "\nThis is a new line.")
```

```Elixir
# 读取文本文件中的内容
File.read("data.txt")
# => "Hello World!\nThis is a new line."
```

## 深入了解

文本文件的历史可以追溯到计算机诞生之初。它是人类交流的一种重要方式，也是计算机编程不可或缺的一部分。除了文本文件，还有其他类型的文件，比如二进制文件。文本文件的主要优点是易于编辑和维护，但是对于存储大量数据来说不够高效。在Elixir中，也可以使用其他库，比如CSV和JSON，来处理特定格式的文本文件。

## 参考资料

[Elixir文档](https://elixir-lang.org/getting-started/file-i-o.html)

[Unicode和文本文件的关系](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/)