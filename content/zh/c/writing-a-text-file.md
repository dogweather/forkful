---
title:                "编写文本文件"
html_title:           "C: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

为什么：写一个文本文件的原因只有一个：它可以帮助我们保存和组织数据。如果你要处理大量的数据，写一个文本文件是一个基本的技能。

如何做：要编写一个文本文件，您需要使用C语言中的“ fprintf”命令来将数据格式化为文本，并使用“ fopen”命令打开一个文件来存储它们。以下是一个简单的例子：

```C
#include <stdio.h>

int main() {
  FILE *fp;
  fp = fopen("data.txt", "w");

  int num = 7;
  float pi = 3.14;
  char str[20] = "Hello World";

  fprintf(fp, "My favorite number is %d\n", num);
  fprintf(fp, "Pi is approximately %.2f\n", pi);
  fprintf(fp, "I love saying %s\n", str);

  fclose(fp);

  return 0;
}
```

输出：

```txt
My favorite number is 7
Pi is approximately 3.14
I love saying Hello World
```

深入探讨：编写文本文件可以提供更大的灵活性，因为它允许您以不同的方式存储和组织数据。您可以使用不同的格式和分隔符来简化数据的处理和分析。此外，文本文件也可以轻松地与其他程序共享和使用。

## 查看更多

- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C语言文本文件处理教程](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C标准库文档](https://devdocs.io/c/)