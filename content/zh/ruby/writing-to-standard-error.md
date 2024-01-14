---
title:    "Ruby: 写入标准错误"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要使用标准错误输出？

大多数程序员的日常工作都涉及修复错误和调试代码。而标准错误输出（Standard Error）可以帮助我们更轻松地跟踪和解决程序中的错误。通过将错误信息打印到标准错误流中，我们可以更方便地捕获和查看程序的错误信息。

## 如何使用标准错误输出？

在Ruby中，我们可以使用 ```$stderr.puts``` 来将错误信息打印到标准错误流中。下面是一个简单的示例代码及其输出：

```Ruby
# 打印错误信息到标准错误流
$stderr.puts "这是一个错误信息"

# 输出：这是一个错误信息
```

在这个例子中，我们使用```puts```命令将错误信息打印到了标准错误流中。另外，我们也可以使用```$stderr.print```或```$stderr.write```来实现同样的效果，只是输出的格式会略有不同。

## 深入了解标准错误输出

除了直接使用```puts```命令外，我们还可以使用```raise```关键字来自动生成错误信息并打印到标准错误流中。下面是一个示例代码及其输出：

```Ruby
# 使用raise关键字自动生成错误信息并打印到标准错误流中
def divide(x, y)
  raise ArgumentError, "除数不能为0" if y == 0
  x / y
end

# 调用divide方法并捕获错误信息
begin
  result = divide(2, 0)
rescue
  $stderr.puts "捕获到错误：#{$!.message}"
end

# 输出：捕获到错误：除数不能为0
```

在这个例子中，我们定义了一个```divide```方法来实现两个数的除法运算。当除数为0时，我们使用```raise```关键字生成一个```ArgumentError```类型的错误信息，并通过标准错误流打印出来。在调用```divide```方法时，我们使用```begin```和```rescue```语句来捕获错误信息并输出。

## 参考资料

- [Ruby标准错误输出官方文档](https://ruby-doc.org/core-2.6.6/IO.html#class-IO-label-Errors+on+IO)
- [更多关于标准错误输出的讨论](https://stackoverflow.com/questions/948135/how-to-write-to-stderr-in-ruby)

## 参见

- [Mandarin Markdown文档格式指南](https://guides.github.com/features/mastering-markdown/#examples)