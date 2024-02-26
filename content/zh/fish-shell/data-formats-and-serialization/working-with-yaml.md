---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:24.324500-07:00
description: "\u901A\u8FC7Fish Shell\u64CD\u4F5CYAML\u6D89\u53CA\u5230\u89E3\u6790\
  \u548C\u64CD\u7EB5YAML\uFF08YAML Ain't Markup Language\uFF0CYAML\u4E0D\u662F\u6807\
  \u8BB0\u8BED\u8A00\uFF09\u6587\u4EF6\uFF0C\u4E00\u79CD\u7528\u4E8E\u914D\u7F6E\u6587\
  \u4EF6\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u5728shell\u73AF\u5883\u7684\u80CC\u666F\u4E0B\u6709\
  \u6548\u5730\u81EA\u52A8\u5316\u548C\u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\u6216\u670D\
  \u52A1\uFF0C\u4FBF\u4E8E\u6267\u884C\u914D\u7F6E\u7BA1\u7406\u548C\u8D44\u6E90\u914D\
  \u7F6E\u7B49\u4EFB\u52A1\u3002"
lastmod: '2024-02-25T18:49:45.847037-07:00'
model: gpt-4-0125-preview
summary: "\u901A\u8FC7Fish Shell\u64CD\u4F5CYAML\u6D89\u53CA\u5230\u89E3\u6790\u548C\
  \u64CD\u7EB5YAML\uFF08YAML Ain't Markup Language\uFF0CYAML\u4E0D\u662F\u6807\u8BB0\
  \u8BED\u8A00\uFF09\u6587\u4EF6\uFF0C\u4E00\u79CD\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u5728shell\u73AF\u5883\u7684\u80CC\u666F\u4E0B\u6709\u6548\
  \u5730\u81EA\u52A8\u5316\u548C\u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\u6216\u670D\u52A1\
  \uFF0C\u4FBF\u4E8E\u6267\u884C\u914D\u7F6E\u7BA1\u7406\u548C\u8D44\u6E90\u914D\u7F6E\
  \u7B49\u4EFB\u52A1\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
