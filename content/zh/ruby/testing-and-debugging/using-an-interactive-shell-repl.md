---
date: 2024-01-26 04:17:21.557381-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u7684REPL\u79F0\u4E3AIRB\uFF08\u4EA4\
  \u4E92\u5F0FRuby\uFF09\u3002\u8DF3\u8FDB\u53BB\uFF0C\u76F4\u63A5\u4ECE\u4F60\u7684\
  \u7EC8\u7AEF\u5C1D\u8BD5Ruby\uFF1A."
lastmod: '2024-04-05T22:38:47.512474-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby\u7684REPL\u79F0\u4E3AIRB\uFF08\u4EA4\
  \u4E92\u5F0FRuby\uFF09\u3002\u8DF3\u8FDB\u53BB\uFF0C\u76F4\u63A5\u4ECE\u4F60\u7684\
  \u7EC8\u7AEF\u5C1D\u8BD5Ruby\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
