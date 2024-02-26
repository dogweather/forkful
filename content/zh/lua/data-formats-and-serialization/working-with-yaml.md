---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:01.104232-07:00
description: "YAML\uFF0C\"YAML Ain't Markup Language\"\u7684\u7F29\u5199\uFF0C\u662F\
  \u4E00\u79CD\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\
  \u6807\u51C6\uFF0C\u901A\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u548C\u8BED\u8A00\
  \u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\
  \u5229\u7528YAML\uFF0C\u662F\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u548C\u53EF\
  \u8BFB\u6027\uFF0C\u4F7F\u5176\u6210\u4E3A\u8BBE\u7F6E\u3001\u591A\u6837\u5316\u5E94\
  \u7528\u914D\u7F6E\u6216\u5E94\u8BE5\u7531\u975E\u7A0B\u5E8F\u5458\u7F16\u8F91\u7684\
  \u5185\u5BB9\u7684\u9996\u9009\u3002"
lastmod: '2024-02-25T18:49:45.502199-07:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\"YAML Ain't Markup Language\"\u7684\u7F29\u5199\uFF0C\u662F\u4E00\
  \u79CD\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\
  \u51C6\uFF0C\u901A\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u548C\u8BED\u8A00\u4E4B\
  \u95F4\u7684\u6570\u636E\u4EA4\u6362\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u5229\
  \u7528YAML\uFF0C\u662F\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\u548C\u53EF\u8BFB\
  \u6027\uFF0C\u4F7F\u5176\u6210\u4E3A\u8BBE\u7F6E\u3001\u591A\u6837\u5316\u5E94\u7528\
  \u914D\u7F6E\u6216\u5E94\u8BE5\u7531\u975E\u7A0B\u5E8F\u5458\u7F16\u8F91\u7684\u5185\
  \u5BB9\u7684\u9996\u9009\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
