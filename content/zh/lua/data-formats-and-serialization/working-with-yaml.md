---
title:                "使用YAML工作"
aliases: - /zh/lua/working-with-yaml.md
date:                  2024-02-03T19:26:01.104232-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？

YAML，"YAML Ain't Markup Language"的缩写，是一种易于人类阅读的数据序列化标准，通常用于配置文件和语言之间的数据交换。程序员之所以利用YAML，是因为它的简单性和可读性，使其成为设置、多样化应用配置或应该由非程序员编辑的内容的首选。

## 如何操作：

Lua并不内置支持YAML，但你可以通过使用第三方库如`lyaml`来操作YAML文件。这个库允许用Lua编码和解码YAML数据。首先，你需要通过Lua的包管理器LuaRocks安装`lyaml`：

```bash
luarocks install lyaml
```

### 解码YAML：

假设你有一个名为`config.yaml`的文件包含以下YAML内容：

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

你可以用以下代码将这个YAML文件解码为一个Lua表：

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

当你运行这个脚本时，它应该输出：

```output
host: localhost
port: 3306
username: user
password: pass
```

### 编码YAML：

要将Lua表编码成YAML格式，你使用`lyaml`提供的`dump`函数。考虑你想创建以下Lua表的YAML表示：

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

输出的YAML将是：

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

按照这些模式，Lua程序员可以有效管理YAML数据，以适用于多种应用程序。这些与YAML的操作对于开发能与系统的其他部分或直接与其他系统顺畅交互的多功能Lua应用程序至关重要。
