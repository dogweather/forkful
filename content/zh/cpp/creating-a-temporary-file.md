---
title:                "创建临时文件"
html_title:           "C++: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么？

创建临时文件是在编程中常见的做法。它可以帮助我们保存一些临时的数据，方便后续使用，同时也可以防止一些冲突和错误。因此，学习如何创建临时文件是非常重要的。

## 如何做？

首先，我们需要包含一个头文件`<cstdio>`以使用文件操作函数。然后，使用`tmpfile()`函数来创建一个临时文件，如下所示：

```C++
FILE* temp = tmpfile();
```

我们还可以使用`fopen()`函数来创建一个带有特定名称的临时文件，如下所示：

```C++
FILE* temp = fopen("temp_file.txt", "w+");
```

现在，我们可以使用标准的文件操作函数来读写这个临时文件。例如，我们可以使用`fprintf()`函数来向文件中写入数据，如下所示：

```C++
fprintf(temp, "Hello, world!");
```

最后，记得使用`fclose()`函数来关闭文件并清理内存。

```C++
fclose(temp);
```

这样，我们便成功地创建了一个临时文件并向它写入了数据。记得在使用完毕后删除这个临时文件，避免占用空间。

## 深入了解

在创建临时文件时，我们需要注意的一点是文件名不能重复。因此，我们可以使用`tmpnam()`函数来生成一个唯一的文件名，如下所示：

```C++
char temp_name[L_tmpnam];
tmpnam(temp_name);
```

另外，我们也可以使用`mkstemp()`函数来创建一个带有唯一名称的临时文件，如下所示：

```C++
char temp_name[] = "temp_file_XXXXXX";
int temp_fd = mkstemp(temp_name);
```
`mkstemp()`函数会在指定名称中替换`X`为随机字符，确保文件名的唯一性。而且，它会返回一个文件描述符，我们可以使用`fdopen()`函数将其转换为`FILE*`类型。

```C++
FILE* temp = fdopen(temp_fd, "w+");
```

这样，我们便可以利用这个临时文件进行更多的操作，例如在写入数据后刷新缓冲区或者获取文件大小等。

# 参考链接

- [C++ Reference - tmpfile()](http://www.cplusplus.com/reference/cstdio/tmpfile/)
- [C++ Reference - fopen()](http://www.cplusplus.com/reference/cstdio/fopen/)
- [C++ Reference - fprintf()](http://www.cplusplus.com/reference/cstdio/fprintf/)
- [C++ Reference - tmpnam()](http://www.cplusplus.com/reference/cstdio/tmpnam/)
- [C++ Reference - mkstemp()](http://www.cplusplus.com/reference/cstdio/mkstemp/)
- [C++ Reference - fdopen()](http://www.cplusplus.com/reference/cstdio/fdopen/)