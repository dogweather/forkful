---
title:                "检查目录是否存在"
html_title:           "Java: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
"检查目录是否存在"是指在写程序时，程序员会使用一种功能来检查指定的目录是否存在。这个功能通常被用于程序运行时，以避免出现错误或异常。程序员会经常使用这个功能来确保程序的可靠性和稳定性。

## 如何：
下面是一个简单的Java代码示例，展示如何检查一个目录是否存在并输出结果：

```Java
import java.io.File;

public class DirectoryCheck {
    public static void main(String[] args) {
        File directory = new File("C:/User/Documents"); //指定目录路径
        if (directory.exists()) { //检查目录是否存在
            System.out.println("目录存在");
        } else {
            System.out.println("目录不存在");
        }
    }
}
```

输出结果将会是：

```
目录存在
```

## 深入解析：
检查目录是否存在的功能已经存在很久了，它最初是在Unix操作系统中被开发出来的。然而，现在它已经在所有主流的编程语言中都被广泛应用。除了Java中使用的File类，其他语言中也有类似的实现方式，比如Python中的os模块和C++中的<dirent.h>库。

除了使用File类来检查目录是否存在，程序员也可以使用其他方式来实现相同的功能。比如，可以使用try-catch语句来捕获可能抛出的异常来验证目录是否存在。但是一般来说，使用File类的exists()方法是最简单和最常用的方法。

## 参考资料：
- [Java File类文档](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
- [Python os模块文档](https://docs.python.org/3/library/os.html)
- [C++ <dirent.h>库文档](https://pubs.opengroup.org/onlinepubs/009696799/basedefs/dirent.h.html)