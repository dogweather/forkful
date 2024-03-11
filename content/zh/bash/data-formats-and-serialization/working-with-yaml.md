---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:41.599409-07:00
description: "YAML\uFF0C\u5373 YAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\u6807\
  \u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\u7684\u6570\
  \u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u4EE5\u7528\u4E8E\u914D\u7F6E\u6587\
  \u4EF6\uFF0C\u4EE5\u53CA\u5728\u5B58\u50A8\u6216\u4F20\u8F93\u6570\u636E\u7684\u5E94\
  \u7528\u4E2D\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u503E\u5411\u4E8E\u4F7F\u7528YAML\uFF0C\
  \u8FD9\u5F52\u56E0\u4E8E\u5B83\u7684\u6E05\u6670\u4E0E\u7B80\u5355\uFF0C\u7279\u522B\
  \u662F\u5728\u6D89\u53CA\u590D\u6742\u914D\u7F6E\u6216\u9700\u8981\u8F7B\u677E\u7F16\
  \u8F91\u6570\u636E\u7ED3\u6784\u7684\u9879\u76EE\u4E2D\u3002"
lastmod: '2024-03-11T00:14:21.783043-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5373 YAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\u6807\u8BB0\
  \u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\u7684\u6570\u636E\
  \u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u4EE5\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\
  \uFF0C\u4EE5\u53CA\u5728\u5B58\u50A8\u6216\u4F20\u8F93\u6570\u636E\u7684\u5E94\u7528\
  \u4E2D\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u503E\u5411\u4E8E\u4F7F\u7528YAML\uFF0C\
  \u8FD9\u5F52\u56E0\u4E8E\u5B83\u7684\u6E05\u6670\u4E0E\u7B80\u5355\uFF0C\u7279\u522B\
  \u662F\u5728\u6D89\u53CA\u590D\u6742\u914D\u7F6E\u6216\u9700\u8981\u8F7B\u677E\u7F16\
  \u8F91\u6570\u636E\u7ED3\u6784\u7684\u9879\u76EE\u4E2D\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

YAML，即 YAML Ain't Markup Language（YAML不是标记语言），是一种人类可读的数据序列化标准，可以用于配置文件，以及在存储或传输数据的应用中使用。程序员倾向于使用YAML，这归因于它的清晰与简单，特别是在涉及复杂配置或需要轻松编辑数据结构的项目中。

## 如何操作：

在 Bash 中直接操作 YAML 需要一点创造性，因为 Bash 没有内置支持解析 YAML。然而，你可以使用外部工具，如 `yq`（一个轻量级且便携的命令行 YAML 处理器），来高效地与 YAML 文件交互。让我们通过一些常见操作：

### 安装 `yq`：

在深入示例之前，请确保你已安装 `yq`。你通常可以通过包管理器安装它，例如，在 Ubuntu 上：

```bash
sudo apt-get install yq
```

或者你可以直接从其 GitHub 仓库下载它。

### 读取值：

假设你有一个名为 `config.yaml` 的文件，内容如下：

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

要读取数据库主机，你可以如下使用 `yq`：

```bash
yq e '.database.host' config.yaml
```

**示例输出：**

```
localhost
```

### 更新值：

要在 `config.yaml` 中更新用户的名称，使用带 `-i`（就地）选项的 `yq eval` 命令：

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

用以下命令验证更改：

```bash
yq e '.user.name' config.yaml
```

**示例输出：**

```
newadmin
```

### 添加新元素：

要在数据库部分下添加一个新字段 `timeout`：

```bash
yq e '.database.timeout = 30' -i config.yaml
```

检查文件内容将确认添加。

### 删除元素：

要移除用户下的密码：

```bash
yq e 'del(.user.password)' -i config.yaml
```

此操作将从配置中删除密码字段。

记住，`yq` 是一款强大的工具，具有更多功能，包括将 YAML 转换为 JSON、合并文件，甚至更复杂的操作。请参考 `yq` 文档以进一步探索。
