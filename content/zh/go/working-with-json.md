---
title:                "Go: 使用json编程"
simple_title:         "使用json编程"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-json.md"
---

{{< edit_this_page >}}

# 为什么使用JSON

JSON是一种轻量级的数据交换格式，它易于阅读和编写，因此被广泛用于各种编程语言中。在Go语言中，我们可以使用内置的`encoding/json`包来处理JSON数据。通过使用JSON，我们可以轻松地在不同的系统和平台之间传递数据，这使得它成为一种非常方便和强大的数据格式。

## 如何使用JSON

首先，我们需要导入`encoding/json`包。然后，我们可以使用`json.Marshal()`函数来将Go数据结构转换为JSON格式的字符串。例如：

```Go
type Student struct {
	Name string
	Age int
	Grade string
}

s := Student{"John", 18, "12th"}
b, err := json.Marshal(s)
fmt.Println(string(b))
```

输出将是`{"Name":"John","Age":18,"Grade":"12th"}`。我们也可以使用`json.MarshalIndent()`函数来输出格式化的JSON字符串，这可以更方便地阅读。例如：

```Go
b, err := json.MarshalIndent(s, "", "  ")
fmt.Println(string(b))
```

输出将是：

```
{
  "Name": "John",
  "Age": 18,
  "Grade": "12th"
}
```

反过来，我们也可以使用`json.Unmarshal()`函数将JSON字符串解析为Go数据结构。例如：

```Go
jsonStr := `{"Name":"Jane","Age":17,"Grade":"11th"}`
var s2 Student
err := json.Unmarshal([]byte(jsonStr), &s2)
fmt.Println(s2.Age)
```

输出将是`17`。所以，我们可以很容易地在Go语言中处理JSON数据。

## 深入了解JSON

在处理JSON数据时，我们需要注意以下几点：

- JSON数据中的键名必须是唯一的，同时也必须是一个字符串。而值可以是字符串、布尔值、数字、对象、数组或空值。
- 如果我们要将JSON数据解析为一个嵌套的结构体，那么结构体的字段必须是公共访问级别（首字母大写）。
- 如果JSON数据中含有不在结构体中定义的额外字段，那么它们将会被忽略。
- 当使用`json.Marshal()`函数时，如果出现了无法识别的数据类型，那么将会返回错误。

更多关于Go语言中如何使用JSON的信息，可以查看官方文档或其他相关教程。

# 查看也可用

- [官方文档：encoding/json](https://golang.org/pkg/encoding/json/)
- [Go语言中处理JSON的教程](https://blog.learngoprogramming.com/decode-json-with-the-go-language-96d0f7cc2874)
- [使用JSON的好处及其在Go语言中的应用](https://medium.com/@kaperys/golang-and-json-9a71f9cf40d6)