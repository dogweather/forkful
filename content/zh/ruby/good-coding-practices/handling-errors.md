---
date: 2024-01-26 00:57:20.421835-07:00
description: "\u600E\u4E48\u505A\uFF1A Ruby \u4F7F\u7528 `begin`\u3001`rescue`\u3001\
  `ensure` \u548C `end` \u6765\u5904\u7406\u9519\u8BEF\u3002\u4F60\u628A\u53EF\u80FD\
  \u51FA\u98CE\u9669\u7684\u4EE3\u7801\u653E\u5728 `begin` \u548C `end` \u4E4B\u95F4\
  \u3002\u5982\u679C\u51FA\u73B0\u9519\u8BEF\uFF0C`rescue` \u5C31\u4F1A\u4ECB\u5165\
  \u3002"
lastmod: '2024-03-13T22:44:48.382143-06:00'
model: gpt-4-1106-preview
summary: "Ruby \u4F7F\u7528 `begin`\u3001`rescue`\u3001`ensure` \u548C `end` \u6765\
  \u5904\u7406\u9519\u8BEF\u3002\u4F60\u628A\u53EF\u80FD\u51FA\u98CE\u9669\u7684\u4EE3\
  \u7801\u653E\u5728 `begin` \u548C `end` \u4E4B\u95F4\u3002\u5982\u679C\u51FA\u73B0\
  \u9519\u8BEF\uFF0C`rescue` \u5C31\u4F1A\u4ECB\u5165."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 怎么做：
Ruby 使用 `begin`、`rescue`、`ensure` 和 `end` 来处理错误。你把可能出风险的代码放在 `begin` 和 `end` 之间。如果出现错误，`rescue` 就会介入。

```Ruby
begin
  # 危险代码放在此处。
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "哎呀！你不能那样做：#{e.message}"
ensure
  puts "无论是否发生错误，这总会运行。"
end
```

示例输出：
```
哎呀！你不能那样做：除以0
无论是否发生错误，这总会运行。
```

## 深入了解
在程序语言史上，错误处理有了显著的进化，早期的语言往往具有粗糙或不存在的机制。Ruby 的异常处理受到像 Python 和 Smalltalk 这样的语言的启发。

Ruby 中 `begin-rescue` 的替代方法包括在方法定义中使用 `rescue` 或使用 `throw` 和 `catch` 进行非标准流控制，尽管它们不用于典型的错误处理。

一个有趣的细节：Ruby 的异常是对象（`Exception` 类及其后代的实例），所以你可以定义自定义错误类并做更多事情 —— 你可以在程序中传递丰富的状态，以实现更健壮的错误处理。

## 另请参阅
- Ruby 文档中关于异常和错误处理的部分：[ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- 关于 Ruby 错误处理最佳实践的详细指南：[thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
