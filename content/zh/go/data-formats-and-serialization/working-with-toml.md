---
aliases:
- /zh/go/working-with-toml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:43.538351-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\uFF0C\u6C64\u59C6\u7684\u660E\
  \u4E86\u3001\u7B80\u6D01\u8BED\u8A00\uFF09\u662F\u4E00\u79CD\u914D\u7F6E\u6587\u4EF6\
  \u683C\u5F0F\uFF0C\u56E0\u5176\u7B80\u5355\u7684\u8BED\u6CD5\u800C\u6613\u4E8E\u9605\
  \u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528TOML\u6765\u914D\u7F6E\u5E94\u7528\u7A0B\
  \u5E8F\u8BBE\u7F6E\u548C\u4F9D\u8D56\u9879\uFF0C\u56E0\u4E3A\u5176\u6E05\u6670\u6027\
  \u548C\u76F4\u63A5\u6620\u5C04\u5230\u6570\u636E\u7ED3\u6784\u7684\u7279\u6027\uFF0C\
  \u4F7F\u5176\u5728\u8BB8\u591AGo\u9879\u76EE\u4E2D\u6210\u4E3A\u8BBE\u7F6E\u548C\
  \u7BA1\u7406\u914D\u7F6E\u7684\u70ED\u95E8\u9009\u62E9\u3002"
lastmod: 2024-02-18 23:08:58.730985
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\uFF0C\u6C64\u59C6\u7684\u660E\u4E86\
  \u3001\u7B80\u6D01\u8BED\u8A00\uFF09\u662F\u4E00\u79CD\u914D\u7F6E\u6587\u4EF6\u683C\
  \u5F0F\uFF0C\u56E0\u5176\u7B80\u5355\u7684\u8BED\u6CD5\u800C\u6613\u4E8E\u9605\u8BFB\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528TOML\u6765\u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\
  \u8BBE\u7F6E\u548C\u4F9D\u8D56\u9879\uFF0C\u56E0\u4E3A\u5176\u6E05\u6670\u6027\u548C\
  \u76F4\u63A5\u6620\u5C04\u5230\u6570\u636E\u7ED3\u6784\u7684\u7279\u6027\uFF0C\u4F7F\
  \u5176\u5728\u8BB8\u591AGo\u9879\u76EE\u4E2D\u6210\u4E3A\u8BBE\u7F6E\u548C\u7BA1\
  \u7406\u914D\u7F6E\u7684\u70ED\u95E8\u9009\u62E9\u3002"
title: "\u4F7F\u7528TOML\u8FDB\u884C\u5DE5\u4F5C"
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
