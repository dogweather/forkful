---
title:                "使用正则表达式"
html_title:           "Ruby: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么要使用正则表达式

正则表达式是一种强大的工具，在编程中可以帮助我们处理和匹配文本信息。它可以帮助我们快速有效地搜索和替换符合特定模式的文本，节省了大量手动操作的时间和精力。因此，学习和使用正则表达式可以使编程工作更加高效和便捷。

## 如何使用正则表达式

下面将通过几个简单的例子来介绍如何在Ruby中使用正则表达式。

### 匹配手机号码

要匹配电话号码，我们可以使用`/`作为正则表达式的分隔符，并在`/`之间输入我们想要匹配的模式。例如，要匹配中国大陆的手机号码，我们可以使用以下正则表达式：

```Ruby
/1[3456789]\d{9}/
```

其中，`1`为手机号码的前缀，`[3456789]`表示第二位数可以是3-9中的任意一个，`\d`表示匹配任意数字，`{9}`表示匹配9个数字，即手机号码的后9位。下面是一个完整的例子：

```Ruby
phone_numbers = ["18600000000", "13800000000", "13900000000", "18900000000"]

phone_numbers.each do |number|
  if number =~ /1[3456789]\d{9}/
    puts "#{number}是一个有效的手机号码"
  else
    puts "#{number}不是一个有效的手机号码"
  end
end

# Output:
# 18600000000是一个有效的手机号码
# 13800000000是一个有效的手机号码
# 13900000000是一个有效的手机号码
# 18900000000是一个有效的手机号码
```

### 替换文本中的指定部分

除了匹配文本，正则表达式也可以用来替换文本中的指定部分。假设我们有一个字符串`"Hello, Ruby!"`，我们想把其中的`"Ruby"`替换为`"Mandarin"`，我们可以这样写：

```Ruby
"Hello, Ruby!".gsub(/Ruby/, "Mandarin")

# Output:
# "Hello, Mandarin!"
```

### 校验邮箱地址

根据邮箱的命名规则，我们可以用以下正则表达式来匹配邮箱地址：

```Ruby
/[\w-]+@[\w-]+(\.[\w-]+)+/
```

其中，`[\w-]+`表示匹配任意字母、数字、下划线或中划线一次或多次，`@`表示匹配`@`符号，`(\.[\w-]+)+`表示匹配以`.`开头的任意字母、数字、下划线或中划线组合一次以上，最后的`+`表示匹配该组合一次或多次。下面是一个完整的例子：

```Ruby
email_addresses = ["example@ruby.com", "user_123@example-com.net", "invalid_email"]

email_addresses.each do |email|
  if email =~ /[\w-]+@[\w-]+(\.[\w-]+)+/
    puts "#{email}是一个有效的邮箱地址"
  else
    puts "#{email}不是一个有效的邮箱地址"
  end
end

# Output:
# example@ruby.com是一个有效的邮箱地址
# user_123@example-com.net是一个有效的邮箱地址
# invalid_email不是一个有效的邮箱地址
```

## 深入了解正则表达式

正则表达式是一个非常广阔的主题，我们上面只介绍了一些基础的用法。如果想深入了解正则表达式，可以参考以下资源：

- [Ruby官方文档中关于正则表达式的部分](https://ruby-doc.org/core-2.6.5/Regexp.html)
- [Ruby正则表达式教程