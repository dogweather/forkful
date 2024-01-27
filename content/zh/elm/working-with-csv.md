---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
处理CSV (逗号分隔值) 是向电子表格导出或从中读取数据的常见方式。程序员处理CSV来与多种应用程序进行数据交换，因为它简单、通用。

## How to: (如何操作：)
Elm中没有内置的CSV库，但你可以使用字符串处理函数来解析CSV数据。不过，推荐的方式是用Elm编写转换逻辑，并将数据传递到其他可以处理CSV的环境。假设我们有如下的CSV字符串：

```
name,age,city
Alice,30,New York
Bob,25,San Francisco
```

我们可以使用以下Elm代码来解析它：

```Elm
type alias Person =
    { name : String
    , age : Int
    , city : String
    }

parseCsv : String -> List Person
parseCsv csv =
    csv
        |> String.split "\n"
        |> List.drop 1
        |> List.map (String.split ",")
        |> List.map (\person -> case person of
            name :: age :: city :: [] ->
                Just { name = name, age = String.toInt age, city = city }
            _ ->
                Nothing
        )
        |> List.filterMap identity
```

`sampleOutput`可能看起来像这样：

```Elm
[
    { name = "Alice", age = 30, city = "New York" },
    { name = "Bob", age = 25, city = "San Francisco" }
]
```

## Deep Dive (深入探索)
历史上，CSV格式因为简单和文本编辑友好而流行。在Elm里直接处理CSV并不常见，因为Elm主要用于客户端应用并关注前端交互。在Web应用中，开发者通常会在服务器端处理CSV，然后将数据以JSON格式传递到前端。其他处理CSV的编程语言选项包括Python的`csv`模块、JavaScript的`PapaParse`库。

## See Also (另请参阅)
- Elm字符串处理文档: [Elm String](http://package.elm-lang.org/packages/elm-lang/core/latest/String)
- 关于数据格式转换的讨论: [Elm Discourse](https://discourse.elm-lang.org/)
