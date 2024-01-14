---
title:                "Java: 检查目录是否存在"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

在编写Java程序时，经常会遇到需要检查目录是否存在的情况。这是因为在操作文件或者目录时，我们需要确保它们已经存在，否则程序可能会出错。

# 如何

```Java
import java.io.File;

public class CheckDirectory {

    public static void main(String[] args) {
        // 要检查的目录路径
        String directoryPath = "C:/Users/UserName/Documents/MyDirectory";

        // 创建File对象
        File directory = new File(directoryPath);

        // 检查目录是否存在
        if (directory.exists()) {
            System.out.println("目录 " + directoryPath + " 存在");
        } else {
            System.out.println("目录 " + directoryPath + " 不存在");
        }
    }
}
```

输出结果：

```
目录 C:/Users/UserName/Documents/MyDirectory 存在
```

以上代码使用java.io包中的File类，通过调用exists()方法来检查目录是否存在。如果目录存在，则返回true，否则返回false。

# 深入探讨

在Java中，检查目录是否存在可以通过多种方式实现。除了上面提到的使用File类的exists()方法外，还可以使用File类的isDirectory()方法来判断一个对象是否为目录。

此外，还可以使用Java的NIO（New Input/Output）包中的Path和Files类来检查目录是否存在。通过Path类的静态方法get()来获取Path对象，然后通过Files类的静态方法exists()来检查目录是否存在。

# 参考链接

- [Java File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java NIO Tutorial](https://www.baeldung.com/java-nio-path)