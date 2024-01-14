---
title:                "Ruby: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么：作为使用Ruby编程语言的程序员，你可能遇到需要从命令行接收参数的情况。无论是用于用户交互，还是在脚本中运行时，读取命令行参数可以使程序更加灵活和智能。

怎么做：实现读取命令行参数的最简单方法是使用内置的ARGV数组。可以通过在命令行中使用“ruby filename.rb argument1 argument2”来传递参数，然后在代码中使用ARGV数组来接收这些参数。下面是一个例子：

```Ruby
# filename.rb
puts "Hello, #{ARGV[0]} and #{ARGV[1]}!"
```
输出：
```
Hello, argument1 and argument2!
```

深入探讨：虽然使用ARGV数组是最简单的方法，但还有其他更高级的方法来读取命令行参数。比如使用OptionParser库可以轻松解析参数并指定相应的选项。此外，也可以使用枚举类来指定可接受的参数，避免程序出现不可预料的行为。

##See Also
- [OptionParser Documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/optparse/rdoc/OptionParser.html)
- [Ruby Enums](https://www.rubyguides.com/2018/10/ruby-enumerable-module/)