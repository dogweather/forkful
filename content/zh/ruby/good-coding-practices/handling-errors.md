---
title:                "处理错误"
aliases:
- /zh/ruby/handling-errors/
date:                  2024-01-26T00:57:20.421835-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/handling-errors.md"
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
