---
date: 2024-01-26 04:10:04.066388-07:00
description: "Ruby \u81EA\u5E26\u4E86\u4E00\u4E2A\u540D\u4E3A `byebug` \u7684\u5185\
  \u7F6E\u8C03\u8BD5\u5668\u3002\u9996\u5148\uFF0C\u5728\u4F60\u7684 Gemfile \u4E2D\
  \u5305\u542B `byebug` \u5E76\u8FD0\u884C `bundle install`\u3002\u7136\u540E\uFF0C\
  \u5C06 `byebug` \u653E\u5728\u4F60\u5E0C\u671B\u4F60\u7684\u7A0B\u5E8F\u6682\u505C\
  \u7684\u5730\u65B9\u3002 ```Ruby require 'byebug' def\u2026"
lastmod: '2024-03-13T22:44:48.378420-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u81EA\u5E26\u4E86\u4E00\u4E2A\u540D\u4E3A `byebug` \u7684\u5185\u7F6E\
  \u8C03\u8BD5\u5668\u3002\u9996\u5148\uFF0C\u5728\u4F60\u7684 Gemfile \u4E2D\u5305\
  \u542B `byebug` \u5E76\u8FD0\u884C `bundle install`\u3002\u7136\u540E\uFF0C\u5C06\
  \ `byebug` \u653E\u5728\u4F60\u5E0C\u671B\u4F60\u7684\u7A0B\u5E8F\u6682\u505C\u7684\
  \u5730\u65B9\u3002 ```Ruby require 'byebug' def\u2026"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
