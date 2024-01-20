---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 是什么，为什么？
字符串插值是将变量的值或代码表达式嵌入到字符串中的过程。程序员之所以这么做，主要是为了方便地构造和格式化字符串。

## 怎么做：
你可以在双引号内的字符串中使用插值：
```Ruby
name = "Andy"
puts "Hello, #{name}" 
```
输出结果：
```
Hello, Andy
```
你也可以插入表达式：
```Ruby
hours = 24
puts "一天有 #{hours * 60 * 60} 秒"
```
输出结果：
```
一天有 86400 秒
```

## 深入研究：
1. 历史背景：Ruby从早期版本开始，就支持了字符串插值，使得构造字符串变得更加灵活和便捷。
2. 替代方案：除了插值方法，还可以使用串联或者`format/sprintf`方法来拼接字符串，但这些方法相对复杂，不如使用字符串插值直接简洁。
3. 实现细节：在Ruby中，字符串插值是通过`Kernel#sprintf`原生方法实现的，这也是为什么你在字符串插值中可以使用格式说明符，如`%s`，`%d`，等等。