---
title:                "使用YAML工作"
aliases: - /zh/ruby/working-with-yaml.md
date:                  2024-02-03T19:26:24.373039-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
