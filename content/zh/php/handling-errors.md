---
title:                "处理错误"
date:                  2024-01-26T00:55:42.566315-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"
programming_language: "PHP"
category:             "PHP"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/handling-errors.md"
---

{{< edit_this_page >}}

## 什么和为什么？
在PHP中处理错误是指管理和响应破坏程序正常流程的条件，比如缺失文件或数据输入有误。程序员处理错误是为了防止程序崩溃和为用户提供更流畅的体验。

## 如何操作：
在PHP中，您可以使用 `try-catch` 代码块来管理错误，并且可以通过自定义错误处理程序和异常来自定义此过程。

```php
// 基础 try-catch 示例
try {
  // 尝试一些有风险的操作
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // 处理错误
  echo "错误: " . $e->getMessage();
}

// 设置自定义错误处理程序
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// 使用异常
class MyException extends Exception {}

try {
  // 执行某些操作并抛出自定义异常
  throw new MyException("自定义错误！");
} catch (MyException $e) {
  // 处理自定义异常
  echo $e->getMessage();
}

// 示例输出：
// 错误: fopen(nonexistentfile.txt): 打开流失败: 没有这样的文件或目录
// 自定义错误！
```

## 深入了解
在过去，PHP中的错误更多是关于警告和通知，它们不会停止脚本执行。随着语言的成熟，它采用了更健全的面向对象错误处理，通过在PHP 5中引入的Exception类。后来，PHP 7推出了Error类，这才区分了错误和异常。

在有 `try-catch` 代码块之前，PHP使用 `set_error_handler()` 来处理错误。`try-catch` 更干净，更现代化。但自定义错误处理程序仍然有其位置，特别是对于遗留代码或当您需要捕获通常不会作为异常的错误时。

在PHP 7+中的 `Throwable` 接口意味着无论是Error还是Exception，您都可以捕获。这很方便，因为现在你不会错过关键的运行时错误，这些错误之前更难以追踪。

PHP内置机制之外的替代方案包括带有自己错误处理系统的库和框架，它们提供了更多特性，比如错误日志记录到文件或展示用户友好的错误页面。

## 另请参阅
- 官方PHP文档关于异常：https://www.php.net/manual/zh/language.exceptions.php
- PHP正确的错误报告方式：https://phptherightway.com/#error_reporting
- PHP手册关于错误处理：https://www.php.net/manual/zh/book.errorfunc.php
