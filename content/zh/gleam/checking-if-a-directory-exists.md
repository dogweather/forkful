---
title:    "Gleam: 检查目录是否存在"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么？

在编程中，有时候我们需要检查某个目录是否存在。这可以帮助我们避免出现错误，并且确保程序按照我们的预期工作。在Gleam中，有一个方便的方法可以实现这一点。下面我们来学习如何做到这一点。

## 如何实现？

首先，让我们来创建一个目录并检查它是否存在。我们将使用Gleam的`Dir.exists`函数，它接受一个路径作为参数并返回一个布尔值来表示目录是否存在。

```Gleam
dir_path = "example"

if Dir.exists(dir_path) {
  println("Direcotry exists!")
} else {
  println("Directory does not exist!")
}
```

输出：
```
Directory exists!
```

现在，让我们来尝试检查一个不存在的目录。

```Gleam
dir_path = "non-existent-directory"

if Dir.exists(dir_path) {
  println("Direcotry exists!")
} else {
  println("Directory does not exist!")
}
```

输出：
```
Directory does not exist!
```

## 深入探讨

在Gleam中，`Dir.exists`函数本质上是通过调用`IO.File.open_exists`函数来实现的。它会尝试打开给定的路径，如果打开成功，则表明该路径是一个已存在的目录。因此，除了检查目录是否存在外，还可以使用`Dir.exists`函数来检查文件是否存在。但是需要注意的是，如果目录或文件没有相应的权限，则会导致打开失败。

## 另请参阅

- [Gleam文档：Dir模块](https://gleam.run/zh/docs/stdlib/dir.html)
- [Gleam文档：IO模块](https://gleam.run/zh/docs/stdlib/io.html)