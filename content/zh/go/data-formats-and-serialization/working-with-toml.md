---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:43.538351-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Go\u4E2D\u5F00\u59CB\u4F7F\
  \u7528TOML\uFF0C\u9996\u5148\u9700\u8981\u5305\u542B\u4E00\u4E2A\u80FD\u591F\u89E3\
  \u6790TOML\u6587\u4EF6\u7684\u5E93\uFF0C\u56E0\u4E3AGo\u6807\u51C6\u5E93\u5E76\u4E0D\
  \u539F\u751F\u652F\u6301TOML\u3002`BurntSushi/toml`\u5305\u662F\u6B64\u7C7B\u7528\
  \u9014\u7684\u6D41\u884C\u9009\u62E9\u3002\u9996\u5148\uFF0C\u786E\u4FDD\u5B89\u88C5\
  \u5B83\uFF1A."
lastmod: '2024-04-05T22:38:46.359468-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Go\u4E2D\u5F00\u59CB\u4F7F\u7528\
  TOML\uFF0C\u9996\u5148\u9700\u8981\u5305\u542B\u4E00\u4E2A\u80FD\u591F\u89E3\u6790\
  TOML\u6587\u4EF6\u7684\u5E93\uFF0C\u56E0\u4E3AGo\u6807\u51C6\u5E93\u5E76\u4E0D\u539F\
  \u751F\u652F\u6301TOML\u3002`BurntSushi/toml`\u5305\u662F\u6B64\u7C7B\u7528\u9014\
  \u7684\u6D41\u884C\u9009\u62E9\u3002\u9996\u5148\uFF0C\u786E\u4FDD\u5B89\u88C5\u5B83\
  \uFF1A."
title: "\u4F7F\u7528TOML\u8FDB\u884C\u5DE5\u4F5C"
weight: 39
---

## 如何操作：
要在Go中开始使用TOML，首先需要包含一个能够解析TOML文件的库，因为Go标准库并不原生支持TOML。`BurntSushi/toml`包是此类用途的流行选择。首先，确保安装它：

```bash
go get github.com/BurntSushi/toml
```

以下是如何使用它的简单示例。假设您有一个名为`config.toml`的配置文件，内容如下：

```toml
title = "TOML示例"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

现在，您需要创建一个反映TOML结构的Go结构体：

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("标题: %s\n", config.Title)
    fmt.Printf("数据库服务器: %s\n", config.Database.Server)
}
```

示例输出：

```
标题: TOML示例
数据库服务器: 192.168.1.1
```

## 深入探讨
TOML是由GitHub的联合创始人之一汤姆·普雷斯顿-沃纳创建的，旨在提供一种直接的配置文件格式，可以轻松映射到哈希表，并且一看就能理解，无需事先了解格式。它与JSON或YAML形成对比，尽管它们也被广泛使用，但由于大括号、引号和缩进问题，对于配置文件来说可能不那么对人类友好。

Go中的`BurntSushi/toml`包是一个强大的库，不仅允许解码还允许编码TOML文件，使其成为需要同时读写此格式配置文件的应用程序的多功能选择。然而，应该注意的是，随着技术的进步和新版Go的推出，像`pelletier/go-toml`这样的替代品出现了，提供了改进的性能和额外的功能，如树操作和查询支持。

虽然TOML对许多应用程序来说是一个很好的选择，但根据应用程序配置的复杂性以及个人或团队偏好，其他格式如YAML或JSON可能更合适，特别是如果配置要求更复杂的数据结构，TOML的冗长特性可能无法优雅地捕获。然而，对于直接、可读和易于编辑的配置，TOML配合Go的强类型系统和上述库，是一个优秀的选择。
