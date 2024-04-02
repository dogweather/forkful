---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:16.245296-07:00
description: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3001\u69CB\u6210\u30D5\u30A1\u30A4\u30EB\u3084\u7570\u306A\u308B\u30C7\u30FC\u30BF\
  \u69CB\u9020\u3092\u6301\u3064\u8A00\u8A9E\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u7528\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u308B\u3001\u4EBA\u9593\
  \u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u6A19\u6E96\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u306E\u5358\u7D14\
  \u3055\u3068\u3001\u8907\u96D1\u306A\u968E\u5C64\u30C7\u30FC\u30BF\u3092\u5BB9\u6613\
  \u306B\u8868\u73FE\u3067\u304D\u308B\u80FD\u529B\u306E\u305F\u3081\u306B\u3001\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.637978-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3001\u69CB\u6210\u30D5\u30A1\u30A4\u30EB\u3084\u7570\u306A\u308B\u30C7\u30FC\u30BF\
  \u69CB\u9020\u3092\u6301\u3064\u8A00\u8A9E\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u7528\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u308B\u3001\u4EBA\u9593\
  \u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u6A19\u6E96\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u306E\u5358\u7D14\
  \u3055\u3068\u3001\u8907\u96D1\u306A\u968E\u5C64\u30C7\u30FC\u30BF\u3092\u5BB9\u6613\
  \u306B\u8868\u73FE\u3067\u304D\u308B\u80FD\u529B\u306E\u305F\u3081\u306B\u3001\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？

YAMLは、「YAML Ain't Markup Language」の略で、構成ファイルや異なるデータ構造を持つ言語間のデータ交換用に一般的に使用される、人間が読みやすいデータ直列化標準です。プログラマーはその単純さと、複雑な階層データを容易に表現できる能力のために、これを使用しています。

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
