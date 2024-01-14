---
title:    "C++: 目录是否存在的检查"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么 ## 

在开发C++程序时，经常需要检查某个特定的目录是否存在。这可以避免在代码中使用不存在的目录，从而有效地提高程序的稳定性和可靠性。

## 如何做 ## 

```C++
#include <iostream>
#include <filesystem> //包含检查目录存在性的标准库

int main() {
    std::string path = "C:\\Users\\Mandarin\\Documents";

    if (std::filesytem::exists(path)) { //检查目录是否存在
        std::cout << "Directory exists!" << std::endl;
    }
    
    return 0;
}
```

运行以上代码，如果目录"C:\Users\Mandarin\Documents"存在，则会输出"Directory exists!"。这是因为我们使用了标准库中的"exists"函数来检查目录的存在性。

## 深入了解 ## 

为了更深入地了解如何检查目录的存在性，我们可以看一下"filesytem"头文件中"exists"函数的实现。这个函数首先会创建一个"directory_entry"对象，然后通过调用该对象的"is_directory"函数来检查目录的存在性。如果目录存在，则返回true；反之则返回false。

另外，我们还可以使用"C++17"版本中引入的"experimental/filesystem"头文件来实现相同的功能。

## 参考链接 ## 

- [C++标准库filesystem库用法详解](https://www.cnblogs.com/ixenos/p/10795788.html)
- [新C++标准中的文件系统操作](https://zhuanlan.zhihu.com/p/28896647)
- [C++ 17：filesystem](https://www.modernescpp.com/index.php/c-17-filesystem)