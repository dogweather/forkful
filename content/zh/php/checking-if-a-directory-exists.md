---
title:    "PHP: 检查目录是否存在"
keywords: ["PHP"]
---

{{< edit_this_page >}}

#为什么要检查目录是否存在？

在PHP编程过程中，经常会遇到需要检查某个目录是否存在的情况。这样做可以确保代码顺利运行，避免因为缺少必要的文件而出现错误。下面将介绍如何通过简单的代码来检查目录是否存在。

##如何检查目录是否存在？

要检查一个目录是否存在，我们可以使用PHP中的`file_exists()`函数来实现。这个函数接受一个参数，即要检查的目录路径。如果这个目录存在，则返回`true`，否则返回`false`。下面是一个例子：

```PHP
$directory = "/home/blog/images"; //要检查的目录路径

if(file_exists($directory)){
    echo "目录存在"; //目录存在时输出
}else{
    echo "目录不存在"; //目录不存在时输出
}
```

如果要检查的目录不存在，也可以通过添加一行代码来创建这个目录：

```PHP
mkdir($directory); //创建目录
```

这样就能够确保目录存在并继续执行后续的代码。

##深入了解检查目录是否存在

其实，`file_exists()`函数也可以用于检查文件是否存在。只需要将文件路径作为参数传入即可。此外，还有一个更加专业的函数`is_dir()`，可以用来判断一个路径是否为目录。

除了以上两种方法，我们还可以使用PHP中的`glob()`函数来列出目录中的文件和子目录。

#参考链接

- [PHP中的file_exists()函数](https://www.php.net/manual/en/function.file-exists.php)
- [准确判断文件或目录是否存在的好方法](https://www.jb51.net/article/32757.htm)
- [PHP中的glob()函数](https://www.php.net/manual/en/function.glob.php)