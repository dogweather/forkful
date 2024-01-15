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

## 为什么

在进行Java编程时，经常会遇到需要在程序中检查某个目录是否存在的情况。这可以帮助我们在处理文件或者资源时避免出现错误。

## 如何做

在Java中，我们可以使用File类的exists()方法来检查目录是否存在。下面是一个简单的示例代码：

```Java
import java.io.File;

public class DirectoryCheckExample {

    public static void main(String[] args) {
        // 定义一个目录路径
        String directoryPath = "C:/Users/User/Downloads";

        File directory = new File(directoryPath);

        // 使用exists()方法检查目录是否存在
        if (directory.exists()) {
            System.out.println("目录存在！");
        } else {
            System.out.println("目录不存在！");
        }
    }
}
```

执行以上代码，如果定义的目录存在，将输出“目录存在！”，如果不存在，则输出“目录不存在！”。

## 深入探讨

除了使用File类的exists()方法，我们也可以使用更复杂的方法来检查目录是否存在。例如，我们可以使用listFiles()方法来获取目录下的所有文件和子目录，然后再判断目录是否为空。我们还可以使用isDirectory()方法来判断一个路径是否为目录。

## 参考链接

- [Java File类文档](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [B站视频教程：Java判断文件或目录是否存在](https://www.bilibili.com/video/BV1qW41177yw?from=search&seid=5826793447592371349)