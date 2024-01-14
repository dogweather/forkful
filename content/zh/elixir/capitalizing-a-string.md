---
title:    "Elixir: 将字符串大写化"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要对字符串进行大小写转换时，使用Elixir的capitalize功能是非常方便和高效的。

## 如何使用

我们可以使用String.capitalize函数来实现字符串的首字母大写。

```Elixir
String.capitalize("hello world")
```

输出：

```
"Hello world"
```

我们也可以指定第二个参数来指定字符串中的某些单词需要大写。

```Elixir
String.capitalize("hello world", [:words])
```

输出：

```
"Hello World"
```

我们还可以使用String.capitalize!/1来直接修改原始字符串。

```Elixir
str = "hello world"
String.capitalize!(str)
```

输出：

```
"Hello world"
```

## 深入了解

String.capitalize函数实际上是调用了String.capitalize/2函数来完成字符串的转换。它会把字符串分割成单词列表，然后针对每个单词使用String.capitalize_word/1函数来进行转换。如果我们想要自定义 capitalize 的行为，可以使用 String.capitalize_word/2 函数来指定一个转换规则。

## 参考链接

- [Elixir官方文档 - String.capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)
- [Elixir官方文档 - String.capitalize_word/1](https://hexdocs.pm/elixir/String.html#capitalize_word/1)
- [Elixir官方文档 - String.capitalize_word/2](https://hexdocs.pm/elixir/String.html#capitalize_word/2)

## 另请参阅

- [Elixir中字符串的处理函数总结](https://www.jianshu.com/p/02c06b09f77e)
- [Elixir String.capitalize速度解析](https://www.jianshu.com/p/88b3795ab5e8)