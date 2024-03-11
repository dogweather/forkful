---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:06.048671-07:00
description: "\u5728 Go \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u5728 Go \u6570\u636E\u7ED3\u6784\u548C JSON \u683C\
  \u5F0F\u4E4B\u95F4\u7F16\u7801\u548C\u89E3\u7801\u6570\u636E\u3002\u8FD9\u9879\u4EFB\
  \u52A1\u5728 Web \u670D\u52A1\u548C API \u4E2D\u65E0\u5904\u4E0D\u5728\uFF0C\u56E0\
  \u4E3A JSON \u4F5C\u4E3A\u4E00\u79CD\u8F7B\u91CF\u7EA7\u3001\u57FA\u4E8E\u6587\u672C\
  \u548C\u65E0\u8BED\u8A00\u4F9D\u8D56\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\
  \u4F7F\u5F97\u5728\u4E0D\u540C\u7F16\u7A0B\u73AF\u5883\u95F4\u7B80\u5355\u5171\u4EAB\
  \u6570\u636E\u6210\u4E3A\u53EF\u80FD\u3002"
lastmod: '2024-03-11T00:14:20.933166-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\
  \u6CD5\uFF09\u6D89\u53CA\u5728 Go \u6570\u636E\u7ED3\u6784\u548C JSON \u683C\u5F0F\
  \u4E4B\u95F4\u7F16\u7801\u548C\u89E3\u7801\u6570\u636E\u3002\u8FD9\u9879\u4EFB\u52A1\
  \u5728 Web \u670D\u52A1\u548C API \u4E2D\u65E0\u5904\u4E0D\u5728\uFF0C\u56E0\u4E3A\
  \ JSON \u4F5C\u4E3A\u4E00\u79CD\u8F7B\u91CF\u7EA7\u3001\u57FA\u4E8E\u6587\u672C\u548C\
  \u65E0\u8BED\u8A00\u4F9D\u8D56\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u4F7F\
  \u5F97\u5728\u4E0D\u540C\u7F16\u7A0B\u73AF\u5883\u95F4\u7B80\u5355\u5171\u4EAB\u6570\
  \u636E\u6210\u4E3A\u53EF\u80FD\u3002"
title: "\u5904\u7406JSON\u6570\u636E"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Go 中处理 JSON（JavaScript 对象表示法）涉及在 Go 数据结构和 JSON 格式之间编码和解码数据。这项任务在 Web 服务和 API 中无处不在，因为 JSON 作为一种轻量级、基于文本和无语言依赖的数据交换格式，使得在不同编程环境间简单共享数据成为可能。

## 如何操作：

在 Go 中，`encoding/json` 包是你进行 JSON 操作的入口，提供将 Go 数据结构转换为 JSON（编组）和反向转换（解组）的机制。以下是一些基本示例，以帮助你开始：

### 编码（编组）

要将 Go 结构体转换为 JSON，你可以使用 `json.Marshal`。请考虑以下 Go 结构体：

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

输出：

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### 解码（解组）

要将 JSON 解析为 Go 数据结构，使用 `json.Unmarshal`：

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

给定之前的结构体 `User`，此代码将 JSON 字符串解析为一个用户实例。

输出：

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## 深入了解

Go 的 `encoding/json` 包提供了一个简单的 API，抽象了处理 JSON 时涉及的许多复杂性。该包在 Go 的早期发展中被引入，反映了 Go 致力于简单性和效率的哲学。然而，`encoding/json` 在运行时使用反射来检查和修改结构体，可能会在 CPU 密集型场景下导致性能不是最优。

像 `json-iterator/go` 和 `ffjson` 这样的替代品已经出现，它们通过生成静态的编组和解组代码，提供更快的 JSON 处理速度。然而，`encoding/json` 仍然是最常用的包，因为它简单、稳健，而且是标准库的一部分，确保了跨 Go 版本的兼容性和稳定性。

尽管相比之下性能较慢，但由于易用性和与 Go 类型系统的集成，`encoding/json` 适用于大多数应用程序。对于那些在性能至关重要的环境中工作的人来说，探索外部库可能是值得的，但对许多人来说，标准库在速度、简单性和可靠性之间达到了正确的平衡。
