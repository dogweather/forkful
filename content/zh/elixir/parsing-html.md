---
title:                "解析HTML"
date:                  2024-01-20T15:31:03.750548-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
解析HTML意味着从网页代码中提取数据。程序员这么做通常是为了获取信息，整合内容或者自动化网页交互。

## How to: (怎么做：)
```Elixir
# 第一步: 添加Floki库
defp deps do
  [
    {:floki, "~> 0.34.0"}
  ]
end

# 第二步: 获取HTML并解析
html = "<html><body><p>Hello, Elixir!</p></body></html>"
{:ok, document} = Floki.parse_document(html)

# 第三步: 使用选择器获取数据
paragraphs = Floki.find(document, "p")
# 输出: [{"p", [], ["Hello, Elixir!"]}]

# 第四步: 提取文本
text_list = Floki.text(paragraphs)
# 输出: ["Hello, Elixir!"]
```

## Deep Dive (深入探讨)
解析HTML有着悠久的历史，最初为了在服务器端处理网页内容。在Elixir中，Floki是基于HTML5解析器Myhtm的一个库，专为高效处理HTML而设计。与正则表达式等替代方法相比，Floki可以确保操作符合HTML文档标准，并提供了更加直观和简洁的API接口。实现细节上，它使用了一种叫做“选择器”的方式来查找和操作HTML元素，类似于在CSS中的用法。

## See Also (另请参阅)
- [Floki GitHub repository](https://github.com/philss/floki)
- [Myhtm GitHub repository](https://github.com/lexborisov/myhtml)
- [HTML5规范](https://html.spec.whatwg.org/)
