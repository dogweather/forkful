---
title:    "Go: 编写文本文件。"
keywords: ["Go"]
---

{{< edit_this_page >}}

# 为什么

在Go编程中，编写文本文件是一个非常常见的任务。通过编写文本文件，我们可以存储数据、创建配置文件和生成日志等等。这些文本文件在我们的应用程序中起着重要的作用，因此掌握如何编写文本文件是很有必要的。

# 如何做

编写文本文件的方法非常简单。首先，我们需要导入`io/ioutil`包，该包提供了一些方便的函数来读取和写入文件。然后，我们使用`ioutil.WriteFile()`函数来创建并写入文本文件。该函数需要三个参数：文件名、待写入的数据和文件权限。下面是一个简单的示例：

```Go
package main

import (
  "io/ioutil"
  "fmt"
)

func main() {
  data := []byte("这是一个文本文件的内容。")

  err := ioutil.WriteFile("example.txt", data, 0644)
  if err != nil {
    fmt.Println(err)
  }
}
```

运行以上代码后，你会发现在同一目录下生成了一个名为`example.txt`的文本文件，其中包含了我们写入的数据。`0644`代表文件的权限，对应的是`-rw-r--r--`，意思是用户可以读写，其他人只能读取。

# 深入探讨

除了使用`ioutil.WriteFile()`函数，我们还可以使用`os.OpenFile()`函数来创建并写入文件。该函数需要三个参数：文件名、打开模式和文件权限。其中打开模式可以是`os.O_WRONLY`（只写）、`os.O_CREATE`（如果文件不存在则创建）和`os.O_APPEND`（末尾追加）。下面是一个使用`os.OpenFile()`函数的示例：

```Go
package main

import (
  "os"
  "fmt"
)

func main() {
  file, err := os.OpenFile("example.txt", os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0666)
  if err != nil {
    fmt.Println(err)
  }

  defer file.Close()

  data := []byte("这是通过os.OpenFile()函数写入的内容。")

  _, err = file.Write(data)
  if err != nil {
    fmt.Println(err)
  }
}
```

# 参考链接

- [Go语言文档 - io/ioutil](https://studygolang.com/pkgdoc)
- [Go语言文档 - os](https://studygolang.com/pkgdoc)
- [How to Write to File in Go?](https://zetcode.com/golang/writefile/)