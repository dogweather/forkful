---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:11.326988-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u9ED8\u8BA4\u4E0D\u5305\u542B\u5185\
  \u5EFA\u7684YAML\u652F\u6301\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\
  \u7B2C\u4E09\u65B9\u5E93\uFF0C\u5982`yamerl`\u6216`yaml_elixir`\u6765\u5904\u7406\
  YAML\u3002\u8FD9\u91CC\uFF0C\u6211\u4EEC\u5C06\u91CD\u70B9\u5173\u6CE8`yaml_elixir`\uFF0C\
  \u56E0\u4E3A\u5B83\u7684\u6613\u7528\u6027\u548C\u5168\u9762\u7684\u7279\u6027\u3002\
  \ \u9996\u5148\uFF0C\u5C06`yaml_elixir`\u6DFB\u52A0\u5230\u4F60\u7684mix.exs\u4F9D\
  \u8D56\u4E2D\uFF1A."
lastmod: '2024-03-13T22:44:47.389619-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u9ED8\u8BA4\u4E0D\u5305\u542B\u5185\u5EFA\u7684YAML\u652F\u6301\u3002\
  \u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u5982\
  `yamerl`\u6216`yaml_elixir`\u6765\u5904\u7406YAML\u3002\u8FD9\u91CC\uFF0C\u6211\u4EEC\
  \u5C06\u91CD\u70B9\u5173\u6CE8`yaml_elixir`\uFF0C\u56E0\u4E3A\u5B83\u7684\u6613\u7528\
  \u6027\u548C\u5168\u9762\u7684\u7279\u6027."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
Elixir默认不包含内建的YAML支持。然而，你可以使用第三方库，如`yamerl`或`yaml_elixir`来处理YAML。这里，我们将重点关注`yaml_elixir`，因为它的易用性和全面的特性。

首先，将`yaml_elixir`添加到你的mix.exs依赖中：

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

然后，运行`mix deps.get`来获取新的依赖。

### 读取 YAML
给定一个简单的YAML文件，`config.yaml`，内容如下：

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

你可以这样读取这个YAML文件并将其转换为Elixir map：

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# 示例使用
Config.read()
# 输出: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### 写入 YAML
要将一个map写回YAML文件：

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# 示例使用
ConfigWriter.write()
# 这将创建或覆盖`new_config.yaml`，内容为指定的内容
```

注意，`yaml_elixir`允许在YAML文件和Elixir数据结构之间直接转换，这使它成为需要处理YAML数据的Elixir程序员的绝佳选择。
