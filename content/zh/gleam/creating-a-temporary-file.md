---
title:    "Gleam: 创建临时文件"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么: 创建临时文件的目的是为了存储程序执行过程中需要暂时保存的数据，从而保证程序的稳定性和性能。临时文件可以在程序执行完毕后自动删除，避免系统资源的浪费。

如何: 下面用Gleam代码块演示如何创建临时文件，并输出文件名。

```Gleam
import gleam/file

let result = file.temp("data.txt")

case result {
    Ok(filename) ->
        filename |> IO.print("Created temporary file: ")
    Error(error) ->
        error |> IO.print("Failed to create temporary file: ")
}
```

输出: 创建临时文件: data.txt

深入了解: 在Gleam中，可以使用file.temp函数来创建临时文件，该函数返回一个Result类型的值，其中Ok分支表示操作成功，Error分支表示操作失败。另外，可以在创建临时文件时指定文件的后缀名、存储路径和权限等信息。

另外，当临时文件的作用已经完成时，可以通过调用file.delete函数来删除文件，从而释放系统资源。

查看也可以: 想了解更多关于Gleam编程的知识，请访问下面的链接：

- Gleam官方网站：https://gleam.run/
- Gleam GitHub仓库：https://github.com/gleam-lang/gleam
- Gleam社区论坛：https://github.com/gleam-lang/gleam/discussions