---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:17:21.557381-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

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
