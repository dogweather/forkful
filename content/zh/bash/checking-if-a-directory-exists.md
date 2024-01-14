---
title:                "Bash: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么要检查是否存在目录？

在Bash编程中，经常需要检查某个目录是否存在。这有助于程序的可靠性和稳定性，因为如果目录不存在，程序可能会出现错误。通过检查目录是否存在，我们可以在程序中添加相应的逻辑处理来防止错误发生。

## 如何做？

在Bash中，我们可以使用`test`命令来检查目录是否存在。以下是一个示例代码：

```Bash
if test -d "/home/user/test_dir"; then
  echo "Directory exists."
else
  echo "Directory does not exist."
fi
```

上面的代码会先检查`/home/user/test_dir`目录是否存在，如果存在则会输出"Directory exists."，否则输出"Directory does not exist."。

## 深入了解

除了使用`test`命令，我们也可以使用`[ -d "/home/user/test_dir" ]`来检查目录是否存在。这两种方法是等效的，只是写法上略有不同，选择一种适合自己的写法即可。

此外，我们也可以使用`-e`来检查文件是否存在，`-f`来检查普通文件是否存在，`-s`来检查文件是否为空，等等。对于这些可以使用的选项，可以在`man test`命令的文档中找到更多信息。

## 同样也可了解

- [Linux Bash：如何使用test来检查文件是否存在](https://www.howtogeek.com/76269/how-to-test-for-file-or-directory-existence-using-bash/)
- [Bash入门教程：文件操作](https://wangdoc.com/bash/basic/files.html)