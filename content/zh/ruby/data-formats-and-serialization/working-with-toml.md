---
date: 2024-01-26 04:26:00.781831-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u5B89\u88C5 `toml-rb`\
  \ gem\u3002\u5B83\u662F Ruby \u4E2D\u89E3\u6790 TOML \u7684\u6D41\u884C\u9009\u62E9\
  \u3002"
lastmod: '2024-04-05T22:38:47.533593-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u9996\u5148\uFF0C\u5B89\u88C5 `toml-rb`\
  \ gem\u3002\u5B83\u662F Ruby \u4E2D\u89E3\u6790 TOML \u7684\u6D41\u884C\u9009\u62E9\
  \u3002"
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作：
首先，安装 `toml-rb` gem。它是 Ruby 中解析 TOML 的流行选择。

```Ruby
gem install toml-rb
```

接下来，读取 TOML 文件：

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

示例输出可能是：

```
我的超棒应用
```

写入 TOML 文件：

```Ruby
require 'toml-rb'

config = {
  'title' => '我的超棒应用',
  'owner' => {
    'name' => '约翰·多',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

检查 `config.toml`，你会看到你的设置，整齐地存储着。

## 深入探讨
TOML，全称为 Tom's Obvious, Minimal Language，是由 GitHub 的联合创始人 Tom Preston-Werner 在 2013 年左右创建的。其主要目标是成为一种直接的格式，易于解析为数据结构。虽然 JSON 非常适合 API，而 YAML 灵活，但 TOML 的特色在于其强调对人类友好。与 YAML 不同，TOML 旨在提供更类似 INI 的结构，许多人发现这种结构更简单且不易出错。

像 JSON、YAML 或 XML 这样的替代方案各有其强项，但在需要轻松由人类和程序同时维护的配置场景中，TOML 蓬勃发展。它不仅简单，而且强制执行严格且易读的格式化。

在技术层面上，为了使用 Ruby 解析 TOML 内容，我们利用像 `toml-rb` 这样的 gem。这个 gem 利用 Ruby 的动态特性，将 TOML 数据转换为原生 Ruby 哈希、数组和其他基本数据结构。这种转换意味着开发人员可以使用熟悉的 Ruby 语义和方法处理 TOML 数据。

## 另请参阅
- TOML 项目和规范：https://toml.io/zh-cn/
- `toml-rb` gem: https://github.com/emancu/toml-rb
- 比较 TOML、YAML 和 JSON：https://blog.theodo.com/2021/08/compare-yml-toml-json/
