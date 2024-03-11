---
date: 2024-01-26 00:57:20.421835-07:00
description: "\u9519\u8BEF\u5904\u7406\u5305\u62EC\u5728\u4EE3\u7801\u4E2D\u9884\u6599\
  \u5230\u610F\u5916\u60C5\u51B5 \u2014\u2014 \u4F18\u96C5\u5730\u7BA1\u7406\u9519\
  \u8BEF\u548C\u95EE\u9898\u800C\u4E0D\u8BA9\u7A0B\u5E8F\u5D29\u6E83\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5728\u51FA\u73B0\u95EE\u9898\u65F6\u63A7\
  \u5236\u6D41\u7A0B\uFF0C\u4EE5\u53CA\u4FDD\u6301\u7528\u6237\u4F53\u9A8C\u7684\u6D41\
  \u7545\u3002"
lastmod: '2024-03-11T00:14:22.194709-06:00'
model: gpt-4-1106-preview
summary: "\u9519\u8BEF\u5904\u7406\u5305\u62EC\u5728\u4EE3\u7801\u4E2D\u9884\u6599\
  \u5230\u610F\u5916\u60C5\u51B5 \u2014\u2014 \u4F18\u96C5\u5730\u7BA1\u7406\u9519\
  \u8BEF\u548C\u95EE\u9898\u800C\u4E0D\u8BA9\u7A0B\u5E8F\u5D29\u6E83\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5728\u51FA\u73B0\u95EE\u9898\u65F6\u63A7\
  \u5236\u6D41\u7A0B\uFF0C\u4EE5\u53CA\u4FDD\u6301\u7528\u6237\u4F53\u9A8C\u7684\u6D41\
  \u7545\u3002"
title: "\u5904\u7406\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么以及为什么？

错误处理包括在代码中预料到意外情况 —— 优雅地管理错误和问题而不让程序崩溃。程序员这么做是为了在出现问题时控制流程，以及保持用户体验的流畅。

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
