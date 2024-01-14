---
title:                "Ruby: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

做什么：为什么要将字符串转换为小写

在 Ruby 编程中，字符串转换是一个常见的操作。有时候，我们需要将字符串中的所有字符转换为小写，那么为什么要这么做呢？通常的原因有两个：一是为了统一字符串的格式，避免大小写不一致导致的错误；二是为了方便字符串的比较，因为大小写敏感的情况下，相同的字符可能被认为不同而造成错误。

## 怎么做

要将字符串转换为小写，在 Ruby 中有几种方法可以实现，我们一起来看看吧！

### 使用 `downcase` 方法

Ruby 中的字符串对象有一个名为 `downcase` 的方法，可以将字符串中的所有字符转换为小写。示例代码如下：

```Ruby
str = "Hello, WORLD!"
puts str.downcase  # 输出 "hello, world!"
```

可以看到，使用 `downcase` 方法后，字符串中所有的字符都被转换为小写了。

### 使用 `downcase!` 方法

除了 `downcase` 方法外，Ruby 中还有一个 `downcase!` 方法，与 `downcase` 方法的唯一不同之处在于，它会修改原来的字符串对象，而不是返回一个新的字符串。示例代码如下：

```Ruby
str = "Hello, WORLD!"
puts str.downcase!  # 输出 "hello, world!"
puts str  # 输出 "hello, world!"
```

可以看到，原来的字符串对象 `str` 已经被修改为小写字符串了。

### 使用正则表达式替换

除了调用字符串对象的方法，我们也可以使用正则表达式来替换字符串中的字符。示例代码如下：

```Ruby
str = "Hello, WORLD!"
puts str.gsub(/[A-Z]/) { |match| match.downcase }  # 输出 "hello, world!"
```

正则表达式 `[A-Z]` 表示匹配所有的大写字符，`gsub` 方法将匹配到的字符替换为小写字符，最终输出转换后的字符串。

## 深入了解

在 Ruby 中，字符串对象有一个 `capitalize` 方法可以将第一个字符转换为大写，但是没有提供相应的 `uncapitalize` 方法来将第一个字符转换为小写。这是因为，Ruby 中的字符串是不可变的，即无法直接对字符串对象进行修改，因此也没有提供相应的方法。

如果我们想要实现 `uncapitalize` 方法，可以通过定义一个新的方法来实现：

```Ruby
class String
  def uncapitalize
    self[0].downcase + self[1..-1]
  end
end

str = "Hello, WORLD!"
puts str.uncapitalize  # 输出 "hello, WORLD!"
```

这里使用数组的切片访问来将第一个字符转换为小写，并将剩余的字符拼接在后面，从而实现 `uncapitalize` 的功能。

## 参考链接

* Ruby 官方文档：[字符串](https://ruby-lang.org/zh_cn/documentation/ruby-from-other-languages)
* Ruby On Rails 代码规范：[文件名和路径](https://ruby-china.org/wiki/filenames-and-paths)

## 参考链接

* Ruby 官方文档：[字符串](https://ruby-lang.org/zh_cn/documentation/ruby-from-other-languages)
* Ruby On Rails 代码规范：[文件名和路径](https://ruby-china.org/wiki/filenames-and-paths)