---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
为什么要处理JSON？JSON（JavaScript Object Notation）是网页和服务间传输数据的主流格式。Go语言里处理JSON可以让你轻松读写结构化数据、与Web API交互。

## How to:
1. 导入`encoding/json`包
2. 创建一个Go结构体映射JSON
3. 使用`json.Marshal`和`json.Unmarshal`

```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}

func main() {
    // JSON编码
    user := User{Name: "张三", Age: 28}
    jsonData, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(jsonData)) // 输出: {"name":"张三","age":28}

    // JSON解码
    var decodedUser User
    err = json.Unmarshal(jsonData, &decodedUser)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", decodedUser) // 输出: {Name:张三 Age:28}
}
```

## Deep Dive
JSON于2001年由Douglas Crockford推广，并迅速成为Web开发的标准数据交换格式。虽有XML、YAML等替代方案，但JSON因其紧凑性和易于解析，在Go语言中至关重要。Go的`encoding/json`包通过反射支持灵活序列化与反序列化，但对性能有一定影响；一些第三方库如`jsoniter`可提供更高效的处理。

## See Also
- 官方文档：[encoding/json package](https://pkg.go.dev/encoding/json)
- JSON标准：[JSON.org](https://www.json.org/json-en.html)
- Go语言性能更好的JSON库：[jsoniter](https://github.com/json-iterator/go)
