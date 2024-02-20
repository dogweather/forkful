---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:24.373039-07:00
description: "YAML\uFF0C\u5373 YAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00 (YAML Ain't\
  \ Markup Language)\uFF0C\u56E0\u5176\u53EF\u8BFB\u6027\u5F3A\uFF0C\u5728 Ruby \u4E2D\
  \u5E7F\u6CDB\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u5E8F\u5217\u5316\
  \u3002\u5F53\u7A0B\u5E8F\u5458\u9700\u8981\u4EE5\u53EF\u8BFB\u4F46\u7ED3\u6784\u5316\
  \u7684\u65B9\u5F0F\u5B58\u50A8\u6216\u4F20\u8F93\u6570\u636E\u5BF9\u8C61\u65F6\uFF0C\
  \u4ED6\u4EEC\u503E\u5411\u4E8E\u4F7F\u7528 YAML\uFF0C\u8FD9\u7B80\u5316\u4E86\u914D\
  \u7F6E\u7BA1\u7406\u3001\u6570\u636E\u5B58\u50A8\u548C\u8DE8\u8BED\u8A00\u6570\u636E\
  \u5171\u4EAB\u7B49\u4EFB\u52A1\u3002"
lastmod: 2024-02-19 22:05:07.459798
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5373 YAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00 (YAML Ain't Markup\
  \ Language)\uFF0C\u56E0\u5176\u53EF\u8BFB\u6027\u5F3A\uFF0C\u5728 Ruby \u4E2D\u5E7F\
  \u6CDB\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u548C\u6570\u636E\u5E8F\u5217\u5316\u3002\
  \u5F53\u7A0B\u5E8F\u5458\u9700\u8981\u4EE5\u53EF\u8BFB\u4F46\u7ED3\u6784\u5316\u7684\
  \u65B9\u5F0F\u5B58\u50A8\u6216\u4F20\u8F93\u6570\u636E\u5BF9\u8C61\u65F6\uFF0C\u4ED6\
  \u4EEC\u503E\u5411\u4E8E\u4F7F\u7528 YAML\uFF0C\u8FD9\u7B80\u5316\u4E86\u914D\u7F6E\
  \u7BA1\u7406\u3001\u6570\u636E\u5B58\u50A8\u548C\u8DE8\u8BED\u8A00\u6570\u636E\u5171\
  \u4EAB\u7B49\u4EFB\u52A1\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？
YAML，即 YAML 不是标记语言 (YAML Ain't Markup Language)，因其可读性强，在 Ruby 中广泛用于配置文件和数据序列化。当程序员需要以可读但结构化的方式存储或传输数据对象时，他们倾向于使用 YAML，这简化了配置管理、数据存储和跨语言数据共享等任务。

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
