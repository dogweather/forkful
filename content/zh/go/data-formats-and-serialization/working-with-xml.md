---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:11.274201-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u8981\u5728 Go \u4E2D\u89E3\u6790 XML\uFF0C\
  \u4F60\u9700\u8981\u4F7F\u7528 `encoding/xml` \u5305\u3002\u8FD9\u4E2A\u5305\u63D0\
  \u4F9B\u4E86\u5C06 XML \u53CD\u5E8F\u5217\u5316\uFF08\u89E3\u6790\uFF09\u4E3A Go\
  \ \u7ED3\u6784\u4F53\u6240\u9700\u7684\u5DE5\u5177\u3002\u4F8B\u5982\uFF0C\u8003\
  \u8651\u4EE5\u4E0B\u8868\u793A\u4E66\u7C4D\u7684 XML \u6570\u636E\uFF1A."
lastmod: '2024-04-05T22:38:46.360719-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u8FDB\u884C\uFF1A \u8981\u5728 Go \u4E2D\u89E3\u6790 XML\uFF0C\
  \u4F60\u9700\u8981\u4F7F\u7528 `encoding/xml` \u5305\u3002\u8FD9\u4E2A\u5305\u63D0\
  \u4F9B\u4E86\u5C06 XML \u53CD\u5E8F\u5217\u5316\uFF08\u89E3\u6790\uFF09\u4E3A Go\
  \ \u7ED3\u6784\u4F53\u6240\u9700\u7684\u5DE5\u5177\u3002\u4F8B\u5982\uFF0C\u8003\
  \u8651\u4EE5\u4E0B\u8868\u793A\u4E66\u7C4D\u7684 XML \u6570\u636E\uFF1A."
title: "\u5904\u7406XML"
weight: 40
---

## 如何进行：


### 在 Go 中解析 XML
要在 Go 中解析 XML，你需要使用 `encoding/xml` 包。这个包提供了将 XML 反序列化（解析）为 Go 结构体所需的工具。例如，考虑以下表示书籍的 XML 数据：

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

要解析这个，定义一个反映 XML 结构的结构体：

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

输出：

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### 在 Go 中生成 XML
为了从 Go 数据结构生成 XML 文档，你再次使用 `encoding/xml` 包。这次你将 Go 结构体序列化为 XML。考虑之前的 `Book` 结构体：

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

输出：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## 深入探讨
XML 的冗长和复杂性导致 JSON 和其他格式在许多应用程序中变得更受欢迎。然而，XML 在表示复杂的层次数据以及它在遗留系统和特定领域（例如，SOAP 服务）中的广泛使用上，确保了其相关性。

Go 中的 `encoding/xml` 包提供了强大的机制来处理 XML，但值得注意的是，它存在限制。例如，处理 XML 命名空间可能很麻烦，并且可能需要比简单使用案例更详细地了解 XML 规范。此外，尽管 Go 的静态类型及其 `encoding/xml` 包的序列化和反序列化能力通常是高效的，开发人员在处理深层嵌套结构或处理不容易映射到 Go 的类型系统上的 XML 文档时可能会遇到挑战。

对于大多数现代应用程序而言，像 JSON 这样的替代品更简单、更高效。然而，在需要使用 XML 的情况下——由于遗留系统、特定行业标准或复杂数据表示需求——Go 的标准库提供了强大的工具来完成工作。一如既往，数据格式的最佳选择取决于应用程序和环境的具体要求。
