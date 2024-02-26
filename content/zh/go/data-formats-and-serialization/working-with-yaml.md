---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:46.452332-07:00
description: "\u5728 Go \u4E2D\u5904\u7406 YAML \u6D89\u53CA\u5230\u89E3\u6790 YAML\uFF08\
  YAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\uFF09\u6587\u4EF6\uFF0C\u8FD9\u662F\u4E00\
  \u79CD\u5BF9\u4EBA\u7C7B\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\
  \uFF0C\u5C06\u5176\u8F6C\u6362\u6210 Go \u6570\u636E\u7ED3\u6784\uFF0C\u53CD\u4E4B\
  \u4EA6\u7136\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5229\u7528\
  \ YAML \u7684\u7B80\u5355\u6027\u548C\u53EF\u8BFB\u6027\u6765\u5904\u7406\u914D\u7F6E\
  \u6587\u4EF6\u3001\u5E94\u7528\u8BBE\u7F6E\uFF0C\u6216\u5728\u4E0D\u540C\u8BED\u8A00\
  \u7F16\u5199\u7684\u670D\u52A1\u548C\u7EC4\u4EF6\u4E4B\u95F4\u8FDB\u884C\u6570\u636E\
  \u4EA4\u6362\u3002"
lastmod: '2024-02-25T18:49:44.801537-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u5904\u7406 YAML \u6D89\u53CA\u5230\u89E3\u6790 YAML\uFF08\
  YAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\uFF09\u6587\u4EF6\uFF0C\u8FD9\u662F\u4E00\
  \u79CD\u5BF9\u4EBA\u7C7B\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\
  \uFF0C\u5C06\u5176\u8F6C\u6362\u6210 Go \u6570\u636E\u7ED3\u6784\uFF0C\u53CD\u4E4B\
  \u4EA6\u7136\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5229\u7528\
  \ YAML \u7684\u7B80\u5355\u6027\u548C\u53EF\u8BFB\u6027\u6765\u5904\u7406\u914D\u7F6E\
  \u6587\u4EF6\u3001\u5E94\u7528\u8BBE\u7F6E\uFF0C\u6216\u5728\u4E0D\u540C\u8BED\u8A00\
  \u7F16\u5199\u7684\u670D\u52A1\u548C\u7EC4\u4EF6\u4E4B\u95F4\u8FDB\u884C\u6570\u636E\
  \u4EA4\u6362\u3002"
title: "\u4F7F\u7528YAML\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 Go 中处理 YAML 涉及到解析 YAML（YAML 不是标记语言）文件，这是一种对人类友好的数据序列化标准，将其转换成 Go 数据结构，反之亦然。程序员这样做是为了利用 YAML 的简单性和可读性来处理配置文件、应用设置，或在不同语言编写的服务和组件之间进行数据交换。

## 如何操作:

要在 Go 中使用 YAML，首先你需要导入一个支持 YAML 解析和序列化的库，因为 Go 的标准库不直接支持 YAML。最受欢迎的库是 "gopkg.in/yaml.v3"。以下是如何开始：

1. **安装 YAML 包：**

```bash
go get gopkg.in/yaml.v3
```

2. **解析 YAML 到 Go 结构体：**

首先，定义一个 Go 结构体，其结构与你的 YAML 数据匹配。

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("用户: %s\n密码: %s\n", config.Database.User, config.Database.Password)
}
```

**示例输出：**

```
用户: admin
密码: secret
```

3. **序列化 Go 结构体到 YAML：**

以下是如何将 Go 结构体转回 YAML。

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct{
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**示例输出：**

```yaml
---
database:
  user: admin
  password: supersecret
```

## 深入探索：

YAML 在软件开发中的使用日益增长，这归因于它易于阅读的格式，使其成为配置文件、文档或数据交换格式的理想选择。与 JSON（它的竞争对手）相比，YAML 提供注释、标量类型和关系特性，提供了一个更丰富的数据序列化框架。然而，它的灵活性和特性以解析的复杂性为代价，如果不小心处理，可能会导致潜在的安全风险（例如，任意代码执行）。

用于 Go 的 "gopkg.in/yaml.v3" 库是一个健壮的 YAML 处理解决方案，实现了易用性和全面特性支持之间的平衡。就目前而言，虽然有其他选择，如 "go-yaml/yaml"（"gopkg.in/yaml.v3" 背后的库），选用哪个版本通常取决于具体的项目需求或个人偏好。当处理大量数据集或性能至关重要的应用程序时，程序员可能会考虑使用像 JSON 这样的更简单格式，因为它们的解析时间和内存开销更小。尽管如此，对于配置文件或设置，在人类可读性和易用性至关重要的地方，YAML 在 Go 生态系统中仍然是一个强有力的竞争者。
