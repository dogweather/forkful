---
title:    "Ruby: 写到标准错误"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

##为什么

在编程世界中，出现错误是非常常见的。作为程序员，我们需要找到一种方法来有效地捕捉和处理这些错误。在 Ruby 中，我们可以使用标准错误（standard error）来帮助我们做到这一点。 通过向标准错误写入信息，我们可以在程序执行时捕捉并记录错误信息，从而更轻松地找出和解决问题。

##如何

要向标准错误写入信息，我们可以使用 `STDERR.puts` 命令。下面是一个示例代码：

```ruby
begin
    # 假设我们希望获取一个不存在的文件的内容
    File.open("not_exist.txt", "r")
rescue => error
    # 使用标准错误写入错误信息
    STDERR.puts "无法打开文件：" + error.to_s
end
```

运行上面的代码后，我们可以在控制台看到以下输出：

```
无法打开文件：No such file or directory @ rb_sysopen - not_exist.txt
```

这里的关键是 `STDERR.puts` 命令，它将错误信息输出到标准错误流中，而不是标准输出流。 这样做可以让我们对错误信息有一个明确的记录，而不会干扰程序的正常输出。

##深入研究

除了使用 `STDERR.puts` 命令，我们还可以使用 `STDERR.write` 命令来将错误信息写入标准错误流。与 `puts` 不同的是，`write` 命令不会自动添加换行符，因此我们可以自己指定错误信息的格式。

另外，有时候我们可能需要将错误信息输出到日志文件或者其他地方，而不是仅仅在控制台中显示。这时，我们可以使用 `STDERR.reopen` 命令来重新定向标准错误流。下面是一个示例：

```ruby
# 将标准错误流定向到指定的日志文件
STDERR.reopen("error.log", "w")
```

通过重新定向标准错误流，我们可以将所有的错误信息写入到 `error.log` 文件中，从而更方便地进行错误分析和调试。

##参考链接

- [Ruby 官方文档：StandardError](https://ruby-doc.org/core-2.6.3/StandardError.html)
- [Ruby Monk：错误处理](https://www.rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-error-handling/lessons/120-catching-exceptions)
- [RubyGuides：如何记录错误信息？](https://www.rubyguides.com/2018/09/ruby-logging/)
- [Ruby China 论坛帖子：如何将标准错误输出保存到文件？](https://ruby-china.org/topics/11909)