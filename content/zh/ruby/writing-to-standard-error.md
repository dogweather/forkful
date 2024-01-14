---
title:                "Ruby: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Why

为什么会写入到标准错误？

当我们在编写程序时，经常会遇到各种错误，比如运行时错误、语法错误等。这些错误都会在控制台中显示，帮助我们调试代码。但是有时候，我们需要将错误信息记录下来，以便后续分析和处理。这时候，就会用到写入到标准错误的方法。

##How To

在Ruby中，我们可以使用`$stderr`对象来将信息写入到标准错误。下面是一个示例代码：

```Ruby
def divide(x, y)
  if y == 0
    $stderr.puts "除数不能为0"
  else
    puts x / y
  end
end

divide(10, 0)
#输出：除数不能为0
```

在上面的例子中，当除数为0时，我们会将错误信息写入到标准错误，而不是在控制台中输出。这样可以让我们知道出现了什么错误，并及时处理。

##Deep Dive

除了简单的将错误信息写入到标准错误，我们还可以通过`$stderr`对象的一些方法来对错误信息进行处理。例如，如果我们想将错误信息写入到文件中，可以使用`$stderr.reopen`方法来重新指定标准错误输出的位置。

另外，我们还可以使用`$stderr.print`和`$stderr.printf`方法来格式化输出错误信息。这些方法和`puts`方法类似，但是可以让我们更灵活地控制输出的格式。

##See Also

了解更多有关写入到标准错误的资料，请参考以下链接：

1. [Ruby官方文档](https://www.ruby-lang.org/en/documentation/)
2. [标准错误（Standard Error）](https://ruby-doc.org/core-2.5.0/StandardError.html)
3. [标准输出（Standard Output）和标准错误（Standard Error）的区别](https://www.huaweicloud.com/articles/a4764bf5ec97c48fba69ef78e05e93f3.html)

以上就是关于写入到标准错误的简要介绍，希望对大家有所帮助！