---
title:    "PHP: 创建临时文件"
keywords: ["PHP"]
---

{{< edit_this_page >}}

为什么：有好几个原因让程序员会创建临时文件。比如，可以用来存储临时数据，创建缓存文件，或者是为了处理大量数据时节省内存。

临时文件可以是临时性的，因为它们只存在于程序执行过程中，而不会永久保存在系统中。这样就可以节省磁盘空间，并避免产生冗余文件。

## 为什么

有时候在编写PHP代码时，可能会需要创建临时文件。临时文件是一种在程序执行过程中暂时保存数据的方法。这样可以避免在代码中频繁地创建和删除文件，从而提高程序的效率。

## 如何

创建临时文件非常简单，只需要使用PHP的内置函数 `tempnam()` 即可。下面是一个示例代码：

```PHP
$tempFile = tempnam(sys_get_temp_dir(), 'prefix_');
echo $tempFile;
```

在上面的代码中，`tempnam()` 函数会创建一个具有唯一文件名的临时文件，并将文件路径保存在变量 `$tempFile` 中。我们可以使用 `echo` 语句来输出这个文件路径。运行上面的代码，你会看到类似这样的输出：

```
/tmp/prefix_kU3vZ7
```

我们也可以自定义临时文件名的前缀，以便更容易识别和管理临时文件。在上面的示例中，我们使用了 `prefix_` 作为前缀。当然，你也可以选择其他前缀，比如你的项目名称或者一些特定的标识符。

另外，当程序执行结束后，临时文件会自动被删除，从而减少了文件管理的工作量。如果你想手动删除，也可以使用 `unlink()` 函数来删除临时文件。下面是一个删除临时文件的示例代码：

```PHP
unlink($tempFile);
```

## 深入了解

除了 `tempnam()` 函数，PHP还提供了其他一些函数用于处理临时文件，比如 `tmpfile()` 和 `tempnam()`。`tmpfile()` 函数会创建一个临时文件并返回一个资源句柄，让我们可以通过这个句柄来操作临时文件。`tempnam()` 函数则与上面提到的函数类似，但是它会让我们指定临时文件的路径和文件名前缀。

另外，我们也可以通过在配置文件中设置 `upload_tmp_dir` 来指定临时文件的存储路径。如果你的程序涉及文件上传，那么这个设置会非常有用。

## 参考资料

- [PHP手册 - 创建临时文件](https://www.php.net/manual/zh/function.tempnam.php)
- [PHP手册 - 临时文件和上传文件](https://www.php.net/manual/zh/features.file-upload.php)
- [PHP手册 - tmpfile()函数](https://www.php.net/manual/zh/function.tmpfile.php)
- [PHP手册 - tempnam()函数](https://www.php.net/manual/zh/function.tempnam.php)

## 参看

如果你想了解更多关于PHP程序设计的内容，可以参考下面这些文章：

- [如何在PHP中使用文件操作](https://www.google.com)
- [PHP中一些有用的内置函数](https://www.google.com)
- [PHP中的错误处理技巧](https://www.google.com)