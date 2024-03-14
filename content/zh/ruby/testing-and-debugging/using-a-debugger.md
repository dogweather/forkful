---
date: 2024-01-26 04:10:04.066388-07:00
description: "\u5728 Ruby \u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\uFF0C\u8D4B\u4E88\u4E86\
  \u7A0B\u5E8F\u5458\u4E00\u79CD\u8D85\u80FD\u529B\uFF0C\u53EF\u4EE5\u6682\u505C\u4ED6\
  \u4EEC\u7684\u4EE3\u7801\u6267\u884C\uFF0C\u68C0\u67E5\u53D8\u91CF\uFF0C\u5E76\u4E14\
  \u9010\u884C\u8DE8\u8FC7\u4ED6\u4EEC\u7684\u4EE3\u7801\u3002\u4EBA\u4EEC\u4F7F\u7528\
  \u5B83\u6765\u6D88\u706D\u9519\u8BEF\uFF0C\u7406\u89E3\u4EE3\u7801\u6D41\u7A0B\uFF0C\
  \u4EE5\u53CA\u5F53\u9B54\u6CD5\u53D1\u751F\u65F6\uFF08\u6216\u4E0D\u53D1\u751F\u65F6\
  \uFF09\u786E\u5207\u5730\u770B\u5230\u4ED6\u4EEC\u6240\u7F16\u5199\u7684\u5492\u8BED\
  \uFF08\u4EE3\u7801\uFF09\u5728\u505A\u4EC0\u4E48\u3002"
lastmod: '2024-03-13T22:44:48.378420-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Ruby \u4E2D\u4F7F\u7528\u8C03\u8BD5\u5668\uFF0C\u8D4B\u4E88\u4E86\
  \u7A0B\u5E8F\u5458\u4E00\u79CD\u8D85\u80FD\u529B\uFF0C\u53EF\u4EE5\u6682\u505C\u4ED6\
  \u4EEC\u7684\u4EE3\u7801\u6267\u884C\uFF0C\u68C0\u67E5\u53D8\u91CF\uFF0C\u5E76\u4E14\
  \u9010\u884C\u8DE8\u8FC7\u4ED6\u4EEC\u7684\u4EE3\u7801\u3002\u4EBA\u4EEC\u4F7F\u7528\
  \u5B83\u6765\u6D88\u706D\u9519\u8BEF\uFF0C\u7406\u89E3\u4EE3\u7801\u6D41\u7A0B\uFF0C\
  \u4EE5\u53CA\u5F53\u9B54\u6CD5\u53D1\u751F\u65F6\uFF08\u6216\u4E0D\u53D1\u751F\u65F6\
  \uFF09\u786E\u5207\u5730\u770B\u5230\u4ED6\u4EEC\u6240\u7F16\u5199\u7684\u5492\u8BED\
  \uFF08\u4EE3\u7801\uFF09\u5728\u505A\u4EC0\u4E48\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 何为调试器及其用途？

在 Ruby 中使用调试器，赋予了程序员一种超能力，可以暂停他们的代码执行，检查变量，并且逐行跨过他们的代码。人们使用它来消灭错误，理解代码流程，以及当魔法发生时（或不发生时）确切地看到他们所编写的咒语（代码）在做什么。

## 如何使用：

Ruby 自带了一个名为 `byebug` 的内置调试器。首先，在你的 Gemfile 中包含 `byebug` 并运行 `bundle install`。然后，将 `byebug` 放在你希望你的程序暂停的地方。

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

运行此脚本将在 `byebug` 处暂停执行，并且你将被带入一个交互式会话，在那里你可以输入诸如：

```
step
next
continue
var local
```

的命令。

示例输出将给你一个看起来像这样的提示：

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## 深入探索：

早在 `byebug` 之前，Ruby 程序员使用 `debugger` 和 `pry`。后者，`pry`，不仅仅是一个调试器；它是一个功能强大的 REPL，也可以用于用 `binding.pry` 断点进行调试。

`byebug` 的替代品包括 `pry-byebug`，它将 `pry` 与 `byebug` 的功能结合起来，以及 `ruby-debug`，这是一个不再活跃维护的旧的 gem。

当你调用 `byebug` 时，调试器会暂停你的代码执行，并让你窥视运行时。你可以看到和改变变量，跳转到代码中的不同点，甚至可以逐行运行一些 Ruby 代码。这有点像为你的 Ruby 代码拥有时光旅行的能力。

## 另见：

- Byebug GitHub 仓库：[https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry 文档：[https://github.com/pry/pry](https://github.com/pry/pry)
- 调试 Rails 应用指南：[https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
