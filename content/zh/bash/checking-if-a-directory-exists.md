---
title:                "检查目录是否存在"
date:                  2024-01-19
simple_title:         "检查目录是否存在"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
检查目录是否存在就是确认在文件系统中该目录是否真实存在。程序员经常需要这个操作来避免错误，比如读取不存在的目录，或者尝试在一个已经存在的目录里创建同名目录。

## How to:
```Bash
if [ -d "$DIRECTORY" ]; then
  echo "The directory $DIRECTORY exists."
else
  echo "The directory $DIRECTORY does not exist."
fi
```
示例输出：
```
The directory /path/to/your/directory exists.
```
或
```
The directory /path/to/your/directory does not exist.
```

## Deep Dive
在Unix和类Unix系统中，检查文件和目录的存在是一项基础功能，几乎所有的shell脚本编程语言都提供了这个能力。Bash使用`-d`标志来特别检查一个目录是否存在。这个标志是`test`命令的一部分，也经常以`[` 作为`test`命令的一个别名。为什么要检测目录存在？因为它可以帮助你决定是否可以继续执行后续的文件操作，从而避免可能出现的运行时错误。

方法的替代品包括使用`mkdir -p`来创建目录，这个命令会创建目录，如果该目录已经存在，它会静默地成功执行。另外，对于复杂的脚本，程序员可能会选择更高级的语言如Python或Ruby来处理文件系统的交互。

关于实现细节，`-d`标志背后，Bash实际上调用的是`stat`系统调用。这个调用会返回文件或目录的元信息，包括类型，然后Bash会检查它是否为目录。

## See Also
- Bash手册：https://www.gnu.org/software/bash/manual/
- `test`命令详细信息：https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html
- `stat`系统调用参考：http://man7.org/linux/man-pages/man2/stat.2.html
