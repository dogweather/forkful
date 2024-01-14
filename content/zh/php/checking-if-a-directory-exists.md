---
title:                "PHP: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#为什么

在编写PHP代码时，有时我们需要检查某个目录是否存在。这对于确保我们的代码正常运行非常重要，特别是当涉及到处理文件和文件夹时。通过检查目录是否存在，我们可以避免出现一些错误，并及时地处理它们。

#如何进行检查

使用PHP中的`file_exists()`函数可以轻松检查目录是否存在。例如，如果我们要检查名为“images”的目录是否存在，我们可以使用以下代码：

```PHP
<?php
if(file_exists("images")){
    echo "目录存在";
} else {
    echo "目录不存在";
}
```

如果目录存在，输出将是“目录存在”，如果不存在，输出将是“目录不存在”。

#深入了解

在检查目录是否存在时，我们需要注意以下两点：

- 使用正确的路径：确保使用正确的路径来检查目录是否存在。如果目录位于根目录下，则可以直接使用目录名称，否则，需要使用相对路径或绝对路径。

- 权限问题：如果当前目录或要检查的目录没有适当的权限，那么`file_exists()`函数将返回`false`，即使目录实际上是存在的。因此，在检查目录是否存在之前，要确保目录具有适当的权限。

#看看这些

了解更多有关PHP中检查目录是否存在的信息，可以参考以下链接：

- [PHP官方文档：file_exists()函数]（https://www.php.net/manual/en/function.file-exists.php）
- [如何检查PHP中的目录是否存在]（https://www.tutorialrepublic.com/faq/how-to-check-if-a-directory-exists-in-php.php）
- [PHP中检查目录是否存在的实例]（https://www.w3schools.com/php/func_filesystem_file_exists.asp）