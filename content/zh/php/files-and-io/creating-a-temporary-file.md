---
date: 2024-01-20 17:41:00.726717-07:00
description: "How to \u5982\u4F55\u64CD\u4F5C\uFF1A \u5728PHP\u4E2D\u521B\u5EFA\u4E00\
  \u4E2A\u4E34\u65F6\u6587\u4EF6\uFF0C\u53EF\u4EE5\u4F7F\u7528`tmpfile()`\u51FD\u6570\
  \uFF0C\u5B83\u4F1A\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u7684\u6587\u4EF6\u5E76\u6253\
  \u5F00\u4F9B\u8BFB\u5199\uFF0C\u6587\u4EF6\u4F7F\u7528\u5B8C\u6210\u540E\u4F1A\u81EA\
  \u52A8\u5220\u9664\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.043436-06:00'
model: gpt-4-1106-preview
summary: "How to \u5982\u4F55\u64CD\u4F5C\uFF1A \u5728PHP\u4E2D\u521B\u5EFA\u4E00\u4E2A\
  \u4E34\u65F6\u6587\u4EF6\uFF0C\u53EF\u4EE5\u4F7F\u7528`tmpfile()`\u51FD\u6570\uFF0C\
  \u5B83\u4F1A\u521B\u5EFA\u4E00\u4E2A\u4E34\u65F6\u7684\u6587\u4EF6\u5E76\u6253\u5F00\
  \u4F9B\u8BFB\u5199\uFF0C\u6587\u4EF6\u4F7F\u7528\u5B8C\u6210\u540E\u4F1A\u81EA\u52A8\
  \u5220\u9664\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## How to 如何操作：
在PHP中创建一个临时文件，可以使用`tmpfile()`函数，它会创建一个临时的文件并打开供读写，文件使用完成后会自动删除。

```php
<?php
$tempFile = tmpfile();
fwrite($tempFile, "临时数据，马上就消失。");
// 重置文件指针位置
fseek($tempFile, 0);
// 输出文件内容
echo fread($tempFile, 1024);
// 文件关闭时，临时文件自动被删除
fclose($tempFile);
?>
```

临时文件会在文件关闭之后被自动删除，所以不需手动清理。

## Deep Dive 深入了解：
创建临时文件是一种安全的数据管理方式，它避免了硬盘上永久存储可能敏感的信息。PHP的`tmpfile()`函数自从PHP 4版本以来就可用，所以它已经成为了标准的实践方法。除了`tmpfile()`, 还有`tempnam()`函数可以创建一个有具体文件名的临时文件，允许您的程序稍后通过文件名访问这个文件。另一种选择是`sys_get_temp_dir()`，这个函数返回用于临时文件的目录，可以和`tempnam()`配合使用。

```php
<?php
$tmpDir = sys_get_temp_dir();
$tmpFile = tempnam($tmpDir, 'TMP_');
fwrite(fopen($tmpFile, 'w'), '持久一点的临时数据。');
echo file_get_contents($tmpFile);
// 删除文件
unlink($tmpFile);
?>
```

当处理大文件或者需要保持文件状态而不立即销毁时，`tempnam()`是个不错的选择。

## See Also 另请参阅：
- [PHP Manual on tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP Manual on tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [PHP Manual on sys_get_temp_dir()](https://www.php.net/manual/en/function.sys-get-temp-dir.php)

在编写临时文件处理功能时，上述链接提供了实用和实现的宝贵信息。
