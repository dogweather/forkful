---
title:    "C++: 创建临时文件"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

当我们编写C++程序时，有时候需要在程序运行过程中临时创建一个文件。这个文件可以用来存储临时数据，完成一些特殊的操作，或者是调试程序。使用临时文件可以帮助我们更有效地管理程序运行时的数据，避免因为数据过多而导致程序崩溃。在下面的文章中，我们将介绍如何在C++中创建临时文件，以及更深入的技术细节。

## 如何创建临时文件

在C++中，我们可以使用标准库中的`tmpfile()`函数来创建一个临时文件。这个函数返回一个指向`FILE`结构的指针，我们可以使用`fputs()`和`fgets()`等函数来读写临时文件中的数据。下面是一个简单的例子，展示了如何使用`tmpfile()`函数创建临时文件，并向其中写入一些数据：

```C++
#include <stdio.h>

int main()
{
    // 使用tmpfile()函数创建临时文件
    FILE *fp = tmpfile();

    // 向临时文件中写入数据
    fputs("Hello, World!", fp);

    // 关闭临时文件
    fclose(fp);

    return 0;
}
```

上面的代码中，我们首先使用`tmpfile()`函数创建了一个临时文件，然后使用`fputs()`函数向其中写入了一串数据，最后使用`fclose()`函数关闭了临时文件。当程序结束后，操作系统会自动删除这个临时文件，所以我们不需要手动删除。

除了使用`tmpfile()`函数外，我们还可以使用`tmpnam()`函数来创建一个临时文件的文件名。这个函数会返回一个指向`char`类型的字符串指针，指向的内容是一个唯一的临时文件名。下面是一个例子，展示了如何使用`tmpnam()`函数创建临时文件名，并将它用作`fopen()`函数的参数：

```C++
#include <stdio.h>

int main()
{
    // 使用tmpnam()函数创建临时文件名
    char *filename = tmpnam(NULL);

    // 使用fopen()函数打开临时文件
    FILE *fp = fopen(filename, "w");

    // 向临时文件中写入数据
    fputs("Hello, World!", fp);

    // 关闭临时文件
    fclose(fp);

    return 0;
}
```

需要注意的是，`tmpnam()`函数会在指定的路径下创建一个临时文件，这个路径如果为`NULL`，则会使用默认的临时文件路径。

## 深入探讨

在C++中，创建临时文件的函数实际上是使用操作系统提供的API来实现的。这些API可以分为两种：

1. `tmpfile()`函数使用`tmpnam()`函数来生成一个临时文件名，并使用`fopen()`函数来创建临时文件。
2. `tmpnam()`函数使用操作系统提供的临时文件目录来创建临时文件名。

不同的操作系统可能会有不同的临时文件目录，可以使用`TMPDIR`环境变量来指定临时文件目录。另外，由于临时文件会在程序结束后被自动删除，所以我们在程序中应该尽量避免使用临时文件来存储重要的数据，以免数据丢失。

## 参考链接

- [C++临时文件的创建与读写](https://www.cnblogs.com/chio/archive/2012/08/19/2640613.html)
- [Tempfile (临时文件) - cppreference.com](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [Tmpnam (临时文件名) - cppreference.com](https://en.cppreference.com/w/cpp/io/c/tmpnam)

## 另请参阅

- [C语言 - 在程序中创建临时文件](https://blog.csdn.net