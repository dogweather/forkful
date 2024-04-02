---
date: 2024-01-26 04:17:21.557381-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216REPL\uFF08\u8BFB\u53D6-\u6C42\u503C\
  -\u6253\u5370 \u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\u80FD\u5B9E\u65F6\u6D4B\u8BD5\
  \u4EE3\u7801\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\u5B9E\u9A8C\u3001\u8C03\
  \u8BD5\uFF0C\u5E76\u5728\u4E0D\u521B\u5EFA\u5B8C\u6574\u811A\u672C\u7684\u60C5\u51B5\
  \u4E0B\u5B66\u4E60Ruby\u7684\u7EC6\u5FAE\u4E4B\u5904\u3002"
lastmod: '2024-03-13T22:44:48.375036-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216REPL\uFF08\u8BFB\u53D6-\u6C42\u503C-\u6253\
  \u5370 \u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\u80FD\u5B9E\u65F6\u6D4B\u8BD5\u4EE3\u7801\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\u5B9E\u9A8C\u3001\u8C03\u8BD5\uFF0C\
  \u5E76\u5728\u4E0D\u521B\u5EFA\u5B8C\u6574\u811A\u672C\u7684\u60C5\u51B5\u4E0B\u5B66\
  \u4E60Ruby\u7684\u7EC6\u5FAE\u4E4B\u5904\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 什么 & 为什么？
交互式shell，或REPL（读取-求值-打印 循环），让你能实时测试代码。程序员使用它来实验、调试，并在不创建完整脚本的情况下学习Ruby的细微之处。

## 如何操作：
Ruby的REPL称为IRB（交互式Ruby）。跳进去，直接从你的终端尝试Ruby：

```Ruby
irb
2.7.0 :001 > puts "你好，Ruby世界！"
你好，Ruby世界！
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## 深入探讨
IRB自Ruby 1.8引入以来，已成为Ruby开发者的主要工具。它受Lisp和Python的交互式shell的启发，将实验性与即时反馈融合在一起。像Pry这样的替代品提供了更多功能，如语法高亮和更强大的调试环境。IRB本身很简单，但可以通过如'irbtools'这样的gems增强功能。IRB处理读取-求值-打印循环的方式是读取每一行输入，将其作为Ruby代码求值，然后打印结果，直到退出这一过程不断循环。

## 另请参阅
- [Ruby的IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [irbtools gem](https://github.com/janlelis/irbtools)
