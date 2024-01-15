---
title:                "创建临时文件"
html_title:           "PHP: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

为什么会创建临时文件？有时候在编程中需要存储一些临时性的数据，而临时文件可以提供一个方便、安全且可靠的解决方案。

## How To

创建临时文件很简单，在PHP中可以通过`tmpfile()`函数来实现。

\`\`\`PHP
<?php
$tempfile = tmpfile();
fwrite($tempfile, "Hello World!");
```

这段代码将会创建一个临时文件并将字符串"Hello World!"写入其中。我们可以通过`fgets()`函数来读取临时文件中的内容并打印出来。

\`\`\`PHP
<?php
$tempfile = tmpfile();
fwrite($tempfile, "Hello World!");
rewind($tempfile);
echo fgets($tempfile); // Output: Hello World!
```

如果需要临时文件的文件名，可以使用`tempnam()`函数来生成一个具有唯一文件名的临时文件。

\`\`\`PHP
<?php
$tempfile = tempnam(sys_get_temp_dir(), "tmp");
echo $tempfile; // Output: /tmp/tmp_jd2187
```

值得注意的是，临时文件在脚本执行结束后会自动被删除，所以不需要我们手动清理。

## Deep Dive

创建临时文件时，`tmpfile()`函数实际上是在服务器的临时目录中创建了一个空文件，然后返回一个可读写的文件指针。而`tempnam()`则是在服务器的临时目录中创建了一个具有唯一文件名的空文件并返回文件路径。因此，临时文件并不会保存在我们指定的路径中，而是存储在服务器的临时目录中。

临时文件通常用来存储临时性的数据，比如登录时的验证码等。在使用`tmpfile()`或`tempnam()`创建临时文件时，我们可以指定一个前缀来方便区分文件用途，比如上面使用的"tmp"前缀。另外，我们也可以指定一个可选的路径来创建临时文件，但是为了保证跨平台兼容性，建议使用`sys_get_temp_dir()`函数来获取系统的临时目录路径。

## See Also

- PHP官方文档：https://www.php.net/manual/zh/function.tmpfile.php
- PHP中文手册：http://php.net/manual/zh/function.tempnam.php