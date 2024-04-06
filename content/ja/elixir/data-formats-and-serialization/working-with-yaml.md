---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:16.245296-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306B\u306F\u30D3\u30EB\u30C8\u30A4\u30F3\u306E\
  YAML\u30B5\u30DD\u30FC\u30C8\u306F\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001`yamerl`\u3084`yaml_elixir`\u306E\u3088\u3046\u306A\
  \u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066YAML\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u3053\u3053\u3067\u306F\u3001\u4F7F\u3044\u3084\u3059\u3055\u3068\u5305\u62EC\
  \u7684\u306A\u6A5F\u80FD\u306E\u305F\u3081\u306B\u3001`yaml_elixir`\u306B\u7126\u70B9\
  \u3092\u5F53\u3066\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:37:49.969435-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Elixir\u306B\u306F\u30D3\u30EB\u30C8\u30A4\u30F3\u306E\
  YAML\u30B5\u30DD\u30FC\u30C8\u306F\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001`yamerl`\u3084`yaml_elixir`\u306E\u3088\u3046\u306A\
  \u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066YAML\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u3053\u3053\u3067\u306F\u3001\u4F7F\u3044\u3084\u3059\u3055\u3068\u5305\u62EC\
  \u7684\u306A\u6A5F\u80FD\u306E\u305F\u3081\u306B\u3001`yaml_elixir`\u306B\u7126\u70B9\
  \u3092\u5F53\u3066\u307E\u3059\u3002 \u6700\u521D\u306B\u3001`yaml_elixir`\u3092\
  mix.exs\u306E\u4F9D\u5B58\u95A2\u4FC2\u306B\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
ElixirにはビルトインのYAMLサポートは含まれていません。しかし、`yamerl`や`yaml_elixir`のようなサードパーティライブラリを使用してYAMLを扱うことができます。ここでは、使いやすさと包括的な機能のために、`yaml_elixir`に焦点を当てます。

最初に、`yaml_elixir`をmix.exsの依存関係に追加します：

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

次に、`mix deps.get`を実行して新しい依存関係を取得します。

### YAMLの読み込み
このような単純なYAMLファイル、`config.yaml`があるとします：

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

このYAMLファイルを読み込んで、Elixirのマップに変換する方法は以下の通りです：

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# サンプルの使用方法
Config.read()
# 出力: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAMLの書き込み
マップをYAMLファイルに書き戻すには：

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

# サンプルの使用方法
ConfigWriter.write()
# これにより、指定された内容で`new_config.yaml`が作成または上書きされます
```

`yaml_elixir`がYAMLファイルとElixirのデータ構造間の直接的な変換を可能にすることに注意してください。これは、YAMLデータを扱う必要のあるElixirプログラマーにとって優れた選択となります。
