---
title:                "YAML を操作する"
aliases:
- /ja/elixir/working-with-yaml/
date:                  2024-02-03T19:25:16.245296-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
