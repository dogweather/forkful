---
title:                "检查目录是否存在"
date:                  2024-01-20T14:58:00.019318-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "PowerShell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在PowerShell中检查目录是否存在是判断给定的路径是否指向一个实际目录的过程。程序员这么做是为了验证路径是否有效，以避免在尝试访问或修改不存在的目录时出错。

## How to (如何做)：

下面是如何在PowerShell中检查目录是否存在的代码示例。

```PowerShell
$path = "C:\SomeDirectory"
if (Test-Path $path -PathType Container) {
    Write-Output "Directory exists."
} else {
    Write-Output "Directory does not exist."
}
```

如果目录存在，你会看到：

```
Directory exists.
```

如果目录不存在，你会看到：

```
Directory does not exist.
```

## Deep Dive (深入了解):

在历史上，PowerShell使用`Test-Path`来检查文件、目录或注册表项是否存在。它非常灵活，通过不同的参数可以对路径的不同类型进行检查。

另一种方法是使用`[System.IO.Directory]::Exists($path)`，这是.NET框架下的方法，也适用于检查目录是否存在。不过，`Test-Path`更符合PowerShell的风格。

在实现上，`Test-Path`可以检验路径是否存在，并可以通过 `-PathType` 参数指定是文件还是目录。 `-PathType Container` 指明我们只在意路径是否为一个容器，即目录。

## See Also (另请参阅):

- [Test-Path 官方文档](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [System.IO.Directory::Exists 方法介绍](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
