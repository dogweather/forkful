---
title:                "使用TOML进行工作"
aliases:
- /zh/go/working-with-toml.md
date:                  2024-02-03T18:12:43.538351-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML进行工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

TOML（Tom's Obvious, Minimal Language，汤姆的明了、简洁语言）是一种配置文件格式，因其简单的语法而易于阅读。程序员使用TOML来配置应用程序设置和依赖项，因为其清晰性和直接映射到数据结构的特性，使其在许多Go项目中成为设置和管理配置的热门选择。

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
