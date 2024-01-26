---
title:                "使用TOML"
date:                  2024-01-26T04:22:25.319640-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-toml.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么?
使用TOML涉及在Go中解析和编码TOML（Tom's Obvious, Minimal Language）文件。程序员选择TOML是因为其可读性强且易于映射到数据结构，非常适合用作配置文件。

## 如何操作:
在Go中使用TOML，你通常会使用像`BurntSushi/toml`这样的库。下面是快速解析TOML配置文件的方法:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("标题: %s, 拥有者: %s\n", config.Title, config.Owner.Name)
}
```

`config.toml`样例:

```Toml
title = "示例TOML"
[owner]
name = "Tom Preston-Werner"
```

样例输出:

```
标题: 示例TOML, 拥有者: Tom Preston-Werner
```

## 深入了解
TOML，由Tom Preston-Werner于2013年推出，旨在成为一种最小化的配置文件格式，由于其明确的语义而易于阅读。Go开发者经常选择使用TOML来进行配置，而不是选择像JSON或YAML这样的替代品，因为其直接性以及能够简单地表示复杂的层级结构。

与YAML相比，YAML具有复杂的特性和潜在的安全问题，TOML的扁平设计减少了复杂性和由于打字错误引起的错误。与JSON不同的是，TOML支持评论，使得配置的行内解释变得更加容易。

在Go中使用TOML时，有一些细微之处需要考虑。结构体标签可以自定义结构体如何映射到TOML结构，你还应该了解TOML数组和内联表是如何解析到Go切片和映射中的。

## 另请参阅
- TOML规范: https://toml.io/en/
- BurntSushi/toml库: https://github.com/BurntSushi/toml
- 配置文件格式的比较: https://www.redhat.com/sysadmin/yaml-toml-json-differences