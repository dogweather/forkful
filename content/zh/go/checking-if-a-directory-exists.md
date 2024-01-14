---
title:                "Go: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么：为什么要检查目录是否存在？

在编程中，我们经常需要获取或操作特定的目录，并且在处理文件或文件夹时，我们必须首先确定它们是否存在。检查目录是否存在可以有效地帮助我们避免不必要的错误或代码中断。

## 如何做：如何在Go中检查目录是否存在？

在Go语言中，我们可以使用`os.Stat()`函数来检查目录是否存在。例如，我们可以使用下面的代码来检查当前目录的子目录"folder"是否存在：

```Go
if _, err := os.Stat("folder"); os.IsNotExist(err) {
	fmt.Println("Folder does not exist.")
} else {
	fmt.Println("Folder exists.")
}
```

在这段代码中，我们首先使用`os.Stat()`函数来获取指定目录的文件信息，如果返回的错误是`os.IsNotExist(err)`，则说明目录不存在。否则，说明目录存在。

## 深入探讨：关于检查目录是否存在的更多信息

在Go语言中，除了使用`os.Stat()`函数外，还可以使用`os.IsNotExist()`函数来判断文件或目录是否存在。如果文件或目录不存在，则返回`true`，否则返回`false`。

此外，我们也可以通过使用`os.IsPermission()`函数来判断访问权限是否被拒绝，如果是，则返回`true`，否则返回`false`。

## 参考链接

- https://golang.org/pkg/os/#Stat
- https://golang.org/pkg/os/#IsNotExist
- https://golang.org/pkg/os/#IsPermission