---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.373039-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby \u5185\u7F6E\u4E86\u4E00\u4E2A\u540D\
  \u4E3A Psych \u7684\u5E93\uFF0C\u7528\u4E8E\u89E3\u6790\u548C\u751F\u6210 YAML\u3002\
  \u8981\u4F7F\u7528\u5B83\uFF0C\u9996\u5148\u9700\u8981\u52A0\u8F7D YAML \u6807\u51C6\
  \u5E93\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\u5E2E\u4F60\u5165\
  \u95E8\uFF1A."
lastmod: '2024-04-05T21:53:48.669518-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
Ruby 内置了一个名为 Psych 的库，用于解析和生成 YAML。要使用它，首先需要加载 YAML 标准库。这里有一个基础示例帮你入门：

```ruby
require 'yaml'

# 将要序列化的哈希
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# 将哈希转换为 YAML
yaml_data = person.to_yaml

puts yaml_data
```

**示例输出：**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

要将 YAML 数据加载回 Ruby 对象：

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**示例输出：**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### 使用第三方库：
尽管标准库足以完成基本任务，但对于复杂需求，你可能会考虑使用像 'safe_yaml' 这样的第三方 gem。要使用这类库，首先必须安装 gem：

```bash
gem install safe_yaml
```

然后，你可以使用它来安全地加载 YAML 数据，减轻了用户控制源对象实例化的风险：

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**示例输出：**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

这种方法增强了你处理 YAML 的安全性，使其成为从不可信来源加载 YAML 的应用程序的不错选择。
