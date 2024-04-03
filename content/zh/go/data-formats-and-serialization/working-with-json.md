---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:06.048671-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Go \u4E2D\uFF0C`encoding/json`\
  \ \u5305\u662F\u4F60\u8FDB\u884C JSON \u64CD\u4F5C\u7684\u5165\u53E3\uFF0C\u63D0\
  \u4F9B\u5C06 Go \u6570\u636E\u7ED3\u6784\u8F6C\u6362\u4E3A JSON\uFF08\u7F16\u7EC4\
  \uFF09\u548C\u53CD\u5411\u8F6C\u6362\uFF08\u89E3\u7EC4\uFF09\u7684\u673A\u5236\u3002\
  \u4EE5\u4E0B\u662F\u4E00\u4E9B\u57FA\u672C\u793A\u4F8B\uFF0C\u4EE5\u5E2E\u52A9\u4F60\
  \u5F00\u59CB\uFF1A #."
lastmod: '2024-03-13T22:44:47.170871-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\uFF0C`encoding/json` \u5305\u662F\u4F60\u8FDB\u884C JSON\
  \ \u64CD\u4F5C\u7684\u5165\u53E3\uFF0C\u63D0\u4F9B\u5C06 Go \u6570\u636E\u7ED3\u6784\
  \u8F6C\u6362\u4E3A JSON\uFF08\u7F16\u7EC4\uFF09\u548C\u53CD\u5411\u8F6C\u6362\uFF08\
  \u89E3\u7EC4\uFF09\u7684\u673A\u5236\u3002\u4EE5\u4E0B\u662F\u4E00\u4E9B\u57FA\u672C\
  \u793A\u4F8B\uFF0C\u4EE5\u5E2E\u52A9\u4F60\u5F00\u59CB\uFF1A\n\n#."
title: "\u5904\u7406JSON\u6570\u636E"
weight: 38
---

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
