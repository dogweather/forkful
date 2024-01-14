---
title:    "Java: 检查目录是否存在"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 为什么要检查目录是否存在

在Java编程中，有时候我们需要检查一个特定的目录是否存在。这可能是因为我们需要在该目录下创建新文件，或者读取已存在的文件。检查一个目录是否存在可以帮助我们避免程序出现错误，同时也可以提升程序的健壮性。

## 如何检查目录是否存在

检查目录是否存在可以使用Java的File类中的exists()方法。下面是一个简单的例子：

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // 定义一个目录路径
        String directoryName = "C:\\Users\\User\\Documents\\myDirectory";

        // 创建一个File对象
        File file = new File(directoryName);

        // 使用exists()方法检查目录是否存在
        if (file.exists()) {
            System.out.println("目录存在。");
        } else {
            System.out.println("目录不存在。");
        }
    }
}
```

运行以上代码，输出结果为 "目录不存在。" 如果该目录存在，则会输出 "目录存在。"

## 深入了解检查目录是否存在

除了使用exists()方法，我们也可以使用isDirectory()方法来检查一个目录是否存在。这个方法会检查给定路径是否为一个目录，如果是则返回true，否则返回false。下面是一个使用isDirectory()方法的例子：

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // 定义一个目录路径
        String directoryName = "C:\\Users\\User\\Documents\\myDirectory";

        // 创建一个File对象
        File file = new File(directoryName);

        // 使用isDirectory()方法检查目录是否存在
        if (file.isDirectory()) {
            System.out.println("该路径是一个目录。");
        } else {
            System.out.println("该路径不是一个目录。");
        }
    }
}
```

运行以上代码，输出结果为 "该路径是一个目录。"

## 参考链接

- [Java文档：File类](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [运行例子的实时演示](https://repl.it/@mandarintest/Java-check-directory-exists)