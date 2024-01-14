---
title:    "Javascript: 写入标准错误"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要将代码写入标准错误的流

当我们在编写Javascript代码时，通常会使用console.log()来输出我们想要的信息。但是，有时候我们需要将一些出错信息或者调试信息输出到一个特定的位置，这时候就会用到标准错误流。通过将代码写入标准错误流，我们可以更容易地跟踪程序的运行，从而更快地发现和解决问题。

## 如何将代码写入标准错误流

在Javascript中，我们可以使用process.stderr.write()来将代码写入标准错误流。下面是一个示例，展示了如何使用process.stderr.write()来输出调试信息到标准错误流，以及如何通过命令行来查看这些信息。

```Javascript
// 输出调试信息到标准错误流
process.stderr.write('这是一个调试信息\n');

// 在命令行中运行程序，将标准错误输出重定向到文件
node index.js 2> debug.log

// 查看debug.log文件中的输出
cat debug.log
```

输出结果如下：

```
这是一个调试信息
```

从上面的例子中可以看出，通过将代码写入标准错误流，我们可以方便地在程序运行过程中输出调试信息，从而更快地找到问题所在。

## 深入了解代码写入标准错误流

除了用于输出调试信息外，代码写入标准错误流还有其他用途。例如，在一些脚本中，我们需要将错误信息输出到标准错误流，以便后续处理。另外，通过使用重定向符号`2>`，我们还可以将标准错误输出导入到一个文件中，从而更方便地记录和分析错误信息。

除了process.stderr.write()方法外，我们还可以使用console.error()来将代码写入标准错误流。这两种方法之间的主要区别是，console.error()会自动在输出的内容前面添加"Error:"前缀，而process.stderr.write()则不会。

## 参考链接

- [process stderr Node.js官方文档](https://nodejs.org/api/process.html#process_process_stderr)
- [console error Node.js官方文档](https://nodejs.org/api/console.html#console_console_error_data_args)
- [标准错误流（标准库）维基百科](https://zh.wikipedia.org/wiki/%E6%A0%87%E5%87%86%E9%94%99%E8%AF%AF%E6%B5%81)

# 参见

- [如何在Node.js中输出错误信息？（英文）](https://stackabuse.com/how-to-output-error-messages-in-node-js/)