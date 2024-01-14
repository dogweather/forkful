---
title:    "PHP: 创建临时文件"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么

我们在编写PHP代码时，经常会遇到需要操作临时文件的情况。可能是为了保存一些临时数据，或者处理一些复杂的任务。但是为什么要使用临时文件呢？让我们来深入了解一下。

## 如何做

我们在PHP中可以通过使用`tmpfile()`函数来创建一个临时文件。

```PHP
$file = tmpfile();
```

使用该函数会在操作系统临时目录下创建一个临时文件，并返回一个资源类型的文件指针。如果我们需要将一些数据写入到临时文件中，可以使用`fwrite()`函数。

```PHP
fwrite($file, "Hello World!");
```

最后，记得要关闭临时文件的指针。

```PHP
fclose($file);
```

现在我们可以查看临时文件的内容了。

```PHP
$file = tmpfile();
fwrite($file, "Hello World!");
fseek($file, 0); // 将文件指针移动到开始位置
echo fread($file, filesize('tmpfile.txt')); // 输出临时文件的内容
fclose($file);
```

输出结果为：

```
Hello World!
```

## 深入

除了使用`tmpfile()`函数，我们还可以使用另一个函数`tmpnam()`来创建一个临时文件。与`tmpfile()`不同的是，它会返回一个字符串类型的临时文件路径。

```PHP
$file = tmpnam(sys_get_temp_dir(), "prefix");
echo $file; // 输出结果为：/tmp/prefix.随机字符串
```

我们可以通过`touch()`函数来创建这个临时文件。

```PHP
touch($file);
```

如果我们需要指定临时文件的扩展名，可以使用`sys_get_temp_dir()`函数来获取系统的临时目录。

```PHP
$file = sys_get_temp_dir().'/prefix.tmp';
echo $file; // 输出结果为：/tmp/prefix.tmp
```

## 参考资料

- [PHP官方文档-创建临时文件](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP官方文档-获取临时目录](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [W3School-PHP fwrite函数](https://www.w3school.com.cn/php/func_filesystem_fwrite.asp)