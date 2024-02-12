---
title:                "使用YAML工作"
aliases: - /zh/elixir/working-with-yaml.md
date:                  2024-02-03T19:25:11.326988-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML，全称是YAML Ain't Markup Language（YAML不是标记语言），是一种人类可读的数据序列化标准，通常用于配置文件和在具有不同数据结构的语言之间的数据交换。程序员之所以使用它，是因为它的简单性以及它能够轻松表示复杂的层次数据。

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
