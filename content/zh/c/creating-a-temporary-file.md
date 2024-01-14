---
title:    "C: 创建临时文件"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么要创建临时文件？

在编写C程序时，您可能会发现需要创建临时文件的情况。临时文件是暂时存储数据的文件，通常在程序运行结束后会被自动删除。创建临时文件可以帮助程序更高效地运行，同时也可以保护您的系统免受不必要的文件占用。

## 如何创建临时文件？

在C语言中，可以使用`tmpfile()`函数来创建临时文件。该函数会在操作系统的临时目录中自动创建一个空白文件，并返回一个指向该文件的指针。下面是一个示例代码：

```C
#include <stdio.h>

int main() {
  FILE *tmp_file;
  char text[] = "This is a temporary file.";

  tmp_file = tmpfile(); // 创建临时文件

  if (tmp_file != NULL) {
    // 向临时文件中写入数据
    fprintf(tmp_file, "%s", text);

    // 从临时文件中读取数据
    rewind(tmp_file);
    fscanf(tmp_file, "%s", text);
    printf("Temporary file contents: %s\n", text);

    // 关闭临时文件
    fclose(tmp_file);
  }

  return 0;
}
```

运行上述代码后，您会发现在临时目录中出现了一个名为`tmp.XXXXXX`的文件，其中`XXXXXX`是随机生成的字符串。临时文件内容为`This is a temporary file.`。

## 深入了解临时文件

创建临时文件的具体实现方式会根据操作系统的不同而有所差异。在Windows系统中，`tmpfile()`函数会创建一个具有临时性质的文件，即文件不会出现在任何目录中，只能通过指针来访问。而在Unix/Linux系统中，`tmpfile()`函数会直接在临时目录中创建一个文件。不同的操作系统也可能会有不同的临时目录地址和文件命名规则。

另外，通过`tmpfile()`函数创建的临时文件会在程序运行结束后自动删除。如果您想要在程序运行期间保留临时文件，可以使用`mkstemp()`函数来创建临时文件。

## 参考链接

- [C语言中的临时文件和临时目录](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [tmpfile()函数文档](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [mkstemp()函数文档](https://www.tutorialspoint.com/c_standard_library/c_function_mkstemp.htm)

# 参见

- [如何在C语言中使用文件操作？](https://www.howtoblog.cn/how-to-use-file-io-in-c-language/)