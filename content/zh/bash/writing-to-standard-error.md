---
title:                "Bash: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要将信息写入标准错误

在编写Bash脚本时，我们经常会遇到需要输出一些信息给用户的情况。通常我们使用标准输出来显示这些信息，但是有时候我们也会使用标准错误来输出。使用标准错误可以帮助我们区分正常输出和错误信息，使得程序更易于调试。

## 如何将信息写入标准错误

要将信息写入标准错误，我们可以使用 `2>&1` 这个重定向符号。这个符号的意思是将标准错误的输出重定向到标准输出。下面是一个例子：

```Bash
#!/bin/bash

echo "这是标准输出"
echo "这是标准错误" >&2
```

运行这个脚本，我们可以看到以下输出：

```
这是标准输出
这是标准错误
```

## 深入了解标准错误

当我们使用 `2>&1` 符号时，标准错误的输出会被重定向到标准输出，但是两者的输出会混合在一起。为了更好地区分它们，我们可以使用 `2>` 符号来将标准错误的输出重定向到一个文件中。例如：

```Bash
#!/bin/bash

echo "这是标准输出"
echo "这是标准错误" >&2

echo "这是标准错误重定向到的文件" 2> error.txt
```

运行这个脚本后，我们可以看到：

```
这是标准输出
这是标准错误重定向到的文件
```

在 `error.txt` 文件中，我们会看到 `这是标准错误` 这句话。

## 参考资料

- [Linux命令之重定向：标准输出、标准错误和管道](https://www.cnblogs.com/zhang-sujuan/p/7783713.html)
- [Bash脚本教程](https://wangdoc.com/bash/)
- [Linux Shell知识](http://c.biancheng.net/cpp/shell/)