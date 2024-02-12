---
title:                "使用YAML进行编程"
aliases:
- /zh/go/working-with-yaml.md
date:                  2024-02-03T18:13:46.452332-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
