---
date: 2024-01-27 16:20:41.568195-07:00
description: "\u4F7F\u7528 Ruby \u7684 CLI\uFF08\u547D\u4EE4\u884C\u754C\u9762\uFF09\
  \u4E00\u884C\u547D\u4EE4\u5B9E\u73B0\u7684\u6587\u4EF6\u5C31\u5730\u7F16\u8F91\u529F\
  \u80FD\uFF0C\u5141\u8BB8\u60A8\u76F4\u63A5\u4ECE\u7EC8\u7AEF\u4FEE\u6539\u6587\u4EF6\
  \uFF0C\u65E0\u9700\u5728\u7F16\u8F91\u5668\u4E2D\u6253\u5F00\u6587\u4EF6\uFF0C\u8FDB\
  \u884C\u66F4\u6539\u5E76\u4FDD\u5B58\u56DE\u53BB\u3002\u8FD9\u9879\u6280\u672F\u5BF9\
  \u4E8E\u5FEB\u901F\u4FEE\u6539\u3001\u6279\u91CF\u66F4\u65B0\u6216\u81EA\u52A8\u5316\
  \u91CD\u590D\u4EFB\u52A1\u6781\u5176\u6709\u7528\uFF0C\u53EF\u4EE5\u8282\u7701\u65F6\
  \u95F4\u548C\u7CBE\u529B\u3002"
lastmod: '2024-03-13T22:44:48.369130-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 Ruby \u7684 CLI\uFF08\u547D\u4EE4\u884C\u754C\u9762\uFF09\u4E00\
  \u884C\u547D\u4EE4\u5B9E\u73B0\u7684\u6587\u4EF6\u5C31\u5730\u7F16\u8F91\u529F\u80FD\
  \uFF0C\u5141\u8BB8\u60A8\u76F4\u63A5\u4ECE\u7EC8\u7AEF\u4FEE\u6539\u6587\u4EF6\uFF0C\
  \u65E0\u9700\u5728\u7F16\u8F91\u5668\u4E2D\u6253\u5F00\u6587\u4EF6\uFF0C\u8FDB\u884C\
  \u66F4\u6539\u5E76\u4FDD\u5B58\u56DE\u53BB\u3002\u8FD9\u9879\u6280\u672F\u5BF9\u4E8E\
  \u5FEB\u901F\u4FEE\u6539\u3001\u6279\u91CF\u66F4\u65B0\u6216\u81EA\u52A8\u5316\u91CD\
  \u590D\u4EFB\u52A1\u6781\u5176\u6709\u7528\uFF0C\u53EF\u4EE5\u8282\u7701\u65F6\u95F4\
  \u548C\u7CBE\u529B\u3002."
title: "\u4F7F\u7528\u547D\u4EE4\u884C\u4E00\u884C\u547D\u4EE4\u5C31\u5730\u7F16\u8F91\
  \u6587\u4EF6"
weight: 32
---

## 如何操作：
Ruby 提供了一种直接从命令行就地编辑文件的简单方法。使用 Ruby 的 `-i` 开关，您可以让 Ruby 直接操作提供的文件。我们通过几个例子来看看这在实际中是如何工作的。假设您有一个名为 `greetings.txt` 的文件，内容如下：

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

您想将 "Hello" 一词替换为 "Hi"。以下是您如何做到这一点：

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

执行此命令后，`greetings.txt` 将更新为：

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

如果您担心可能会弄乱数据，Ruby 为您提供了保护。通过为 `-i` 开关提供扩展名，Ruby 在执行更改之前会创建一个备份。例如：

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

现在，除了您编辑过的 `greetings.txt`，在同一个目录下，您将找到一个 `greetings.txt.bak`，里面保存着原始内容。

## 深入了解
Ruby 的就地文件编辑之魔力来源于其结合了 Perl 式的文本处理能力和 Ruby 自身的语法优雅。历史上，Perl 是用于快速一行脚本编写的首选语言，特别是对于文本操作。Ruby 采用了这一范式，允许强大的命令行脚本编写能力。

在其他语言中，存在就地编辑的替代方法，例如 Perl 本身和 Unix 系统中的流编辑器 sed。每种都有其优势——Perl 因其文本处理能力而闻名，而 sed 在流编辑任务的简单性方面无与伦比。然而，Ruby 提供了平衡，尤其是对那些已经熟悉 Ruby 的人来说，它提供了强大的文本操作能力以及更加可读和用户友好的语法。

在实现方面，Ruby 的就地编辑工作方式是通过重命名原始文件，创建一个具有原始文件名的新文件，然后在从重命名的原文件读取时将更改写入这个新文件。这种方法确保了操作的原子性；要么整个文件被成功处理，要么不做任何更改，从而在编辑过程中保护了您数据的完整性。这种机制，结合 Ruby 的异常处理，还提供了针对中断的韧性，如电源故障或进程终止，确保至少备份保持完整。

总之，Ruby 的就地文件编辑彰显了其作为一种脚本语言的实用性，为直接从命令行进行文本操作任务提供了力量、简单性和优雅的完美结合。
