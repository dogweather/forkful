---
title:    "Fish Shell: 检查目录是否存在"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编写Fish Shell程序时，有时候我们需要在代码中检查一个目录是否存在。这样做可以确保我们的程序能够正常运行，并避免产生错误。

## 如何

使用Fish Shell提供的`test`命令可以轻松检查一个目录是否存在。我们可以通过`test`命令的`-d`选项来判断目录是否存在，如果存在则返回`true`，不存在则返回`false`。

```Fish Shell
if test -d /path/to/directory
  echo "目录存在"
else
  echo "目录不存在"
end
```

## 深入探讨

除了使用`test`命令，我们还可以使用Fish Shell的内置函数`is_dir`来检查目录是否存在。`is_dir`函数会返回`true`或`false`，并且它会自动处理目录路径中的符号链接。

```Fish Shell
set directory /path/to/symlink

if is_dir $directory
  echo "目录存在"
else
  echo "目录不存在"
end
```

另外，我们也可以使用`ls`命令来列出一个目录下的文件和目录，并结合`contains`函数来判断目录是否存在。

```Fish Shell
if contains (ls /path/to/directory) my_file
  echo "目录存在"
else
  echo "目录不存在"
end
```

## 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [Linuxize: Check if Directory Exists in Bash](https://linuxize.com/post/how-to-check-if-directory-exists-in-bash/)
- [命令行资料库：test命令](https://wangchujiang.com/linux-command/c/test.html)

## 链接

### 阅读更多Fish Shell编程相关文章

- [掌握Fish Shell命令行技巧](https://blog.csdn.net/EthanWhite/article/details/120733134)
- [Fish Shell的强大功能介绍](https://www.jianshu.com/p/0db995d33906)
- [使用Fish Shell实现自定义命令和函数](https://www.cnblogs.com/MuchNotes/p/13481140.html)