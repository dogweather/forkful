---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:24.324500-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell\u6CA1\u6709\u5185\u7F6E\u652F\
  \u6301\u89E3\u6790YAML\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528\u7B2C\u4E09\u65B9\
  \u5DE5\u5177\uFF0C\u5982`yq`\uFF08\u4E00\u4E2A\u8F7B\u91CF\u7EA7\u4E14\u4FBF\u643A\
  \u7684\u547D\u4EE4\u884CYAML\u5904\u7406\u5668\uFF09\u6765\u5904\u7406YAML\u6570\
  \u636E\u3002 **\u5B89\u88C5yq\uFF08\u5982\u679C\u5C1A\u672A\u5B89\u88C5\uFF09\uFF1A\
  **."
lastmod: '2024-04-05T22:38:47.428233-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell\u6CA1\u6709\u5185\u7F6E\u652F\u6301\
  \u89E3\u6790YAML\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528\u7B2C\u4E09\u65B9\u5DE5\
  \u5177\uFF0C\u5982`yq`\uFF08\u4E00\u4E2A\u8F7B\u91CF\u7EA7\u4E14\u4FBF\u643A\u7684\
  \u547D\u4EE4\u884CYAML\u5904\u7406\u5668\uFF09\u6765\u5904\u7406YAML\u6570\u636E\
  \u3002 **\u5B89\u88C5yq\uFF08\u5982\u679C\u5C1A\u672A\u5B89\u88C5\uFF09\uFF1A**."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
Fish Shell没有内置支持解析YAML，但你可以利用第三方工具，如`yq`（一个轻量级且便携的命令行YAML处理器）来处理YAML数据。

**安装yq（如果尚未安装）：**
```fish
sudo apt-get install yq
```

**从YAML文件中读取值：**
假设你有一个含以下内容的YAML文件`config.yaml`：
```yaml
database:
  host: localhost
  port: 3306
```

要读取数据库主机，你可以使用：
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**示例输出：**
```
localhost
```

**更新YAML文件中的值：**
要将`port`更新为`5432`，使用：
```fish
yq e '.database.port = 5432' -i config.yaml
```
**验证更新：**
```fish
yq e '.database.port' config.yaml
```
**示例输出：**
```
5432
```

**写入新的YAML文件：**
为了创建一个带有预定义内容的新`new_config.yaml`：
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
这使用`yq`来处理并美化打印（-P标记）字符串到一个新的YAML文件。

**解析复杂结构：**
如果你有一个更复杂的YAML文件，并且需要获取嵌套的数组或对象，你可以：
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**示例输出：**
```
server1
server2
```
使用`yq`，Fish Shell使得浏览YAML文档并为各种自动化和配置任务操纵它们变得简单明了。
