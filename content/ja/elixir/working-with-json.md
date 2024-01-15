---
title:                "「JSONでの作業」"
html_title:           "Elixir: 「JSONでの作業」"
simple_title:         "「JSONでの作業」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ JSON を使うのか

JSONは現在、Web開発やデータ通信において広く使われているフォーマットです。この形式を扱うことで、データの取得や処理が容易になり、柔軟性の高いアプリケーションの開発が可能となります。

## 使い方

まずは、JSONモジュールをインポートします。

```Elixir
import Jason
```

### オブジェクトへのアクセス

JSONデータは多くの場合、オブジェクトと配列のネストされた形式で表されます。オブジェクトのキーを指定することで、特定の値にアクセスすることができます。

```Elixir
data = ~s({"name": "John", "age": 25})
name = Jason.get(data, "name")
# => "John"
```

### 変換

Elixirでは、JSON形式のデータをElixirのデータ構造に変換することができます。

```Elixir
data = ~s([{"name": "John", "age": 25}, {"name": "Sarah", "age": 30}])
list = Jason.decode(data)
# => [%{"name" => "John", "age" => 25}, %{"name" => "Sarah", "age" => 30}]
```

### 出力

Elixirでは、データ構造をJSON形式に変換することもできます。

```Elixir
data = %{name: "John", age: 25}
output = Jason.encode!(data)
# => "{\"name\":\"John\",\"age\":25}"
```

## ディープダイブ

JSONモジュールでは、オプション引数を使用することで、より詳細な操作が可能となります。また、パフォーマンスの向上のため、Jasonテンプレートを使用することもできます。

## See Also

- [Jasonモジュールドキュメント](https://hexdocs.pm/jason/Jason.html)
- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/Kernel.html#Jason)
- [Elixir ForumでのJSONについてのディスカッション](https://elixirforum.com/t/json-in-elixir/3377)