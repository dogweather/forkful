---
title:                "使用csv的工作"
html_title:           "Elm: 使用csv的工作"
simple_title:         "使用csv的工作"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-csv.md"
---

{{< edit_this_page >}}

# 为什么要使用CSV

CSV是一种非常常见的数据格式，它被广泛用于存储和传输表格数据。在现代软件开发中，CSV文件经常被用作数据交换的一种方式。如果你想要处理、分析或展示来自不同来源的数据，那么CSV就是你无可替代的工具。

## 如何操作CSV

在Elm中处理CSV非常简单。首先，我们需要导入 `elm-csv`包。然后，我们就可以使用`Csv.Decode`模块来解析CSV数据，利用`Csv.Encode`模块来生成CSV文件。

让我们看一个例子，假设有一个CSV文件包含着学生的考试成绩数据。我们想要从中筛选出及格的学生，并计算他们的平均分数。我们可以使用以下代码来实现：

```
import Csv
import Csv.Decode exposing (Decoder)
import Csv.Encode exposing (encode)

type alias Student = {
    name : String,
    score : Float
}

csvDecoder : Decoder (List Student)
csvDecoder =
    Csv.Decode.csv
        |> Csv.Decode.filter (\row -> Csv.Decode.field 1 row > 60)
        |> Csv.Decode.andMap (\row ->
            { name = Csv.Decode.field 0 row
            , score = Csv.Decode.field 1 row
            }
        )

main : Html msg
main =
    Csv.Decode.decodeFile csvDecoder "students.csv"
        |> Result.map
            (\students ->
                students
                    |> List.map .score
                    |> List.average
            )
        |> Result.map (\avg -> "The average score of passing students is: " ++ toString avg)
        |> Either.fold
            (\err -> div [] [ text "Error: " ++ err ])
            (\success -> div [] [ text success ])
```

上面的代码将会从`students.csv`文件中读取数据，并根据我们设定的条件筛选出及格学生，再计算他们的平均分数。最后，代码将会输出一个HTML元素包含着平均分数。

## 深入了解CSV

CSV文件中的每一行被解析为一个记录（record），每一列被解析为一个字段（field）。在处理CSV时，我们需要考虑数据类型的转换，例如将`String`类型转换为`Float`类型等。`elm-csv`包提供了许多方便的函数来帮助我们处理这些转换。

此外，`elm-csv`还支持读取和解析包含换行符和双引号等特殊字符的CSV文件。它也提供了错误处理机制来帮助我们发现潜在的数据格式错误。

# 参考链接

- [elm-csv GitHub仓库](https://github.com/elm-explorations/csv)
- [Elm语言官方文档](https://guide.elm-lang.org/)