---
title:                "编写文本文件"
html_title:           "Elm: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

写入文本文件是一种在Elm编程中非常常见的操作。它允许我们将数据保存到文件中，以便在程序运行结束后依然可以访问。这对于持久化我们的应用程序状态或者存储用户的数据非常有用。

## 如何

为了写入文本文件，我们首先需要使用 Elm 的 [文件系统模块](https://package.elm-lang.org/packages/elm/file/latest/)。我们使用该模块中的 `file` 函数来创建文件并将数据写入其中。

以下是一个示例代码，我们将 "Hello World!" 写入到名为 `hello.txt` 的文件中：

```Elm
import File
import File exposing (write)

main =
  File.write "hello.txt" "Hello World!"
```

我们可以通过运行以上代码来创建一个名为 `hello.txt` 的文件，并将 "Hello World!" 的字符串写入其中。我们也可以使用 `write` 函数来将任何数据类型转换为字符串进行写入。

## 深入了解

为了更深入了解如何写入文本文件，我们可以进一步探索文件系统模块的其他函数。例如，我们可以使用 `writeAppend` 函数来将数据追加到一个已经存在的文件中，而不是覆盖文件中的现有数据。

我们还可以使用 `writeLines` 函数来将一个字符串列表写入文本文件的多行中，每行一个字符串。这可以帮助我们将结构化的数据保存到文件中，并在需要时轻松地读取它们。

## 参考链接

- [文件系统模块文档](https://package.elm-lang.org/packages/elm/file/latest/)