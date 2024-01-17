---
title:                "使用 YAML 进行编程"
html_title:           "Gleam: 使用 YAML 进行编程"
simple_title:         "使用 YAML 进行编程"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# 什么是 YAML？为什么程序员会用它？

YAML（YAML Ain't Markup Language）是一种轻量级的数据序列化语言，可用于将数据以易读的形式存储和传输。程序员通常会使用YAML来编写配置文件和存储结构化数据，因为它可以比传统的XML和JSON格式更易读和编写。

# 如何使用 YAML：

使用YAML编写配置文件非常简单。用几行代码来展示如何定义一个简单的YAML文件：

```Gleam
title: My Awesome Website
authors:
  - John Smith
  - Jane Doe
```

这里我们定义了一个标题为"My Awesome Website"的网页，并列出了两位作者的名字。当我们运行这段代码时，我们会得到一个YAML数据结构的输出：

```Gleam
%{title: "My Awesome Website", authors: ["John Smith", "Jane Doe"]}
```

我们可以在这个数据结构上进行操作，比如取出标题或者添加新的作者。

# 深入了解：

YAML最初是由Clark Evans于2001年推出的，并基于JSON的语法。它的目标是提供一个比JSON更易读和编写的数据格式，但同时保持和XML一样的表现力。与大多数编程语言相比，YAML的语法相对简单，读起来也更接近自然语言。

虽然XML和JSON也可以用于存储结构化数据，但YAML的语法更加简洁和易读，并且支持更多的数据类型。此外，YAML也可以与其他编程语言无缝集成，因此在很多项目中都是首选的配置文件格式。

# 参考资料：

了解更多关于YAML的知识，请访问以下链接：

- YAML官方网站：https://yaml.org/
- YAML语言规范：https://yaml.org/spec/1.2/spec.html
- YAML在Gleam中的实现：https://github.com/gleam-lang/yaml

祝使用愉快！