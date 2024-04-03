---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:41.599409-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Bash \u4E2D\u76F4\u63A5\u64CD\u4F5C\
  \ YAML \u9700\u8981\u4E00\u70B9\u521B\u9020\u6027\uFF0C\u56E0\u4E3A Bash \u6CA1\u6709\
  \u5185\u7F6E\u652F\u6301\u89E3\u6790 YAML\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\
  \u4F7F\u7528\u5916\u90E8\u5DE5\u5177\uFF0C\u5982 `yq`\uFF08\u4E00\u4E2A\u8F7B\u91CF\
  \u7EA7\u4E14\u4FBF\u643A\u7684\u547D\u4EE4\u884C YAML \u5904\u7406\u5668\uFF09\uFF0C\
  \u6765\u9AD8\u6548\u5730\u4E0E YAML \u6587\u4EF6\u4EA4\u4E92\u3002\u8BA9\u6211\u4EEC\
  \u901A\u8FC7\u4E00\u4E9B\u5E38\u89C1\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:47.986300-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u76F4\u63A5\u64CD\u4F5C YAML \u9700\u8981\u4E00\u70B9\
  \u521B\u9020\u6027\uFF0C\u56E0\u4E3A Bash \u6CA1\u6709\u5185\u7F6E\u652F\u6301\u89E3\
  \u6790 YAML\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u5916\u90E8\u5DE5\
  \u5177\uFF0C\u5982 `yq`\uFF08\u4E00\u4E2A\u8F7B\u91CF\u7EA7\u4E14\u4FBF\u643A\u7684\
  \u547D\u4EE4\u884C YAML \u5904\u7406\u5668\uFF09\uFF0C\u6765\u9AD8\u6548\u5730\u4E0E\
  \ YAML \u6587\u4EF6\u4EA4\u4E92\u3002\u8BA9\u6211\u4EEC\u901A\u8FC7\u4E00\u4E9B\u5E38\
  \u89C1\u64CD\u4F5C\uFF1A\n\n#."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
