---
title:                "检查目录是否存在"
html_title:           "C++: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

在编写程序时，我们经常需要检查某个目录是否存在。这个步骤是非常重要的，因为它可以帮助我们避免一些潜在的错误，比如尝试访问不存在的目录而导致程序崩溃。因此，学习如何检查目录是否存在是很有用的技能，可以增强我们的代码健壮性。

## 如何操作

要检查一个目录是否存在，我们可以使用C++中的`opendir()`函数和`stat()`结构体。首先，我们需要包含`<dirent.h>`和`<sys/stat.h>`头文件。然后，在`main()`函数中使用`opendir()`函数打开所需的目录，并将返回的指针保存在一个`DIR`类型的变量中。接下来，我们可以使用`stat()`结构体中的`st_mode`成员来检查目录的存在性。如果这个值为0，那么目录是存在的；如果为-1，那么表示目录不存在。

```C++
#include <dirent.h>
#include <sys/stat.h>

int main() {
    DIR* dir = opendir("path/to/directory"); // 打开目录
    struct stat dir_info; // 创建结构体变量
    if (stat("path/to/directory", &dir_info) == 0) { // 检查目录是否存在
        cout << "目录存在" << endl;
    } else { // 如果目录不存在
        cout << "目录不存在" << endl;
    }
    closedir(dir); // 关闭目录
    return 0;
}
```

如果我们希望打印出目录中的文件列表，我们可以使用`dirent.h`头文件中的`struct dirent`结构体来获取文件信息。下面是一个示例代码：

```C++
#include <dirent.h>

int main() {
    DIR* dir = opendir("path/to/directory"); // 打开目录
    struct dirent* dir_info; // 创建结构体指针
    while ((dir_info = readdir(dir)) != NULL) { // 读取目录中的文件
        cout << dir_info->d_name << endl; // 打印文件名
    }
    closedir(dir); // 关闭目录
    return 0;
}
```

如果目录存在，输出会类似于这样：

```
file1.txt
file2.txt
file3.txt
```

## 深入了解

`opendir()`函数和`stat()`结构体是C++中常用的用于检查目录是否存在的方法。但是，它们并不是唯一的方法。我们还可以使用`boost/filesystem.hpp`头文件中的`exists()`函数来判断一个目录是否存在。这个函数会返回一个布尔值，如果目录存在则为`true`，否则为`false`。

另外，我们也可以使用C++17中新增的`filesystem`库来检查目录是否存在。在这个库中，我们可以使用`exists()`函数和`path`类来判断目录的存在性。使用这个库的好处是我们不再需要使用C风格的函数，而是可以像调用普通函数一样使用它们。

# 参考链接

- [C++ opendir()函数](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ stat()函数](https://www.tutorialspoint.com/unix_system_calls/stat.htm)
- [C++ boost/filesystem.hpp头文件](https://theboostcpplibraries.com/boost.filesystem-directory-contents)
- [C++17 filesystem库](https://google.github.io/styleguide/cppguide.html#Static_and_Global_Variables)