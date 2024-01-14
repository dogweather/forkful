---
title:    "Java: 检查目录是否存在"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

程序员们常常需要检查一个文件夹是否存在，这是因为有时候他们需要在代码中创建新的文件夹或者处理现有的文件夹。如果一个文件夹不存在，那么相应的代码就会出现错误。

# 如何

在Java中，要检查一个文件夹是否存在，我们使用File类的exists()方法。让我们来看一个简单的例子：

```Java
import java.io.File;

public class CheckDirectoryExists {
    public static void main(String[] args) {

        // 创建一个File对象，指向想要检查的文件夹
        File directory = new File("C:/Users/User/Documents");

        // 使用exists()方法来检查文件夹是否存在
        if (directory.exists()) {
            System.out.println("文件夹存在");
        } else {
            System.out.println("文件夹不存在");
        }
    }
}
```

输出结果：

``` 
文件夹存在
```

# 深入学习
exists()方法是通过检查给定路径是否存在来判断文件夹是否存在的。它可以用来检查任何类型的文件，包括文件夹和文本文件。如果文件夹不存在，exists()方法会返回false。

除了exists()方法，Java中还有其他一些方法可以用来检查文件夹是否存在，比如isDirectory()方法和isFile()方法。这些方法都在File类中定义。

另外，如果需要创建新的文件夹，我们可以使用mkdir()方法或者mkdirs()方法。mkdir()方法用来创建单层文件夹，mkdirs()方法用来创建多层文件夹。

# 查看也许

- [Java File类文档](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [How to check if a directory exists in Java](https://attacomsian.com/blog/check-if-directory-exists-java)

# 总结

通过使用exists()方法和其他相关方法，我们可以轻松地检查一个文件夹是否存在，并在需要时创建新的文件夹。这对于编写Java代码来处理文件非常重要。希望这篇文章对您有所帮助！