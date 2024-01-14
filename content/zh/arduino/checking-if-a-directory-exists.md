---
title:                "Arduino: 检查目录是否存在"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##为什么
在进行程序编程时，有时候我们会需要检查某个目录是否存在。这一步在Arduino编程中也是非常重要的，因为它可以帮助我们避免发生意外的错误。

##如何
要检查一个目录是否存在，我们可以使用Arduino的FileSystem库中的exists()函数。下面是一个简单的示例代码：

```Arduino
if(FileSystem.exists("/data")) {
  // 如果目录存在，则执行某些操作
  Serial.println("目录存在！");
} else {
  // 如果目录不存在，则执行其他操作
  Serial.println("目录不存在！");
}
```

这里，我们使用exists()函数来检查根目录下是否存在名为“data”的目录。如果目录存在，则会输出“目录存在！”的信息；如果不存在，则会输出“目录不存在！”的信息。

##深入了解
文件系统是Arduino的一个常用库，它包含了许多用于管理文件和目录的函数。其中，exists()函数用于检查某个文件或目录是否存在。它的用法比较简单，只需要在函数中传入要检查的路径就可以了。

此外，FileSystem库还包括其他常用函数，如mkdir()用于创建新目录，remove()用于删除文件或目录等。

##参考链接
- [Arduino官方文档：FileSystem库](https://www.arduino.cc/en/Reference/FileSystem)
- [一起学习Arduino之文件系统库](https://www.arduino.cn/thread-95863-1-1.html)
- [Arduino文件系统库教程](https://blog.csdn.net/buchaow22/article/details/107948122)

##参见
- [Arduino编程新手指南](https://bbs.arduino.cc/forum.php?mod=viewthread&tid=707006)
- [如何使用Arduino的SD卡和文件系统库](https://www.ge-m.io/post/how-to-using-arduino-sd-file)
- [Arduino的常用库及其使用方法](https://blog.csdn.net/zhangyongtao2050/article/details/84966754)