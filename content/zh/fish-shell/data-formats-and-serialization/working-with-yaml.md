---
title:                "使用YAML工作"
aliases: - /zh/fish-shell/working-with-yaml.md
date:                  2024-02-03T19:25:24.324500-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
通过Fish Shell操作YAML涉及到解析和操纵YAML（YAML Ain't Markup Language，YAML不是标记语言）文件，一种用于配置文件的数据序列化格式。程序员这样做是为了在shell环境的背景下有效地自动化和配置应用程序或服务，便于执行配置管理和资源配置等任务。

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
