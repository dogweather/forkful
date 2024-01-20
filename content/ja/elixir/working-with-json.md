---
title:                "「jsonを扱う」"
html_title:           "Elixir: 「jsonを扱う」"
simple_title:         "「jsonを扱う」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## なに & なぜ？

JSON(JavaScript Object Notation)は、データのフォーマットであり、Javascriptにおいてオブジェクトを扱う際に一般的に使われています。JSONは人間にとって読みやすく、機械にとっても解析しやすいため、プログラマーにとってはとても便利なツールです。

## 作り方：

Elixirを使ってJSONを作成するには、`Jason`モジュールを用いることができます。以下は簡単な例です。

```
Elixir iex> Jason.encode!(%{name: "John", age: 25})
"{\"name\":\"John\",\"age\":25}"
```

また、JSONを読み込む際には以下のようにします。

```
Elixir iex> Jason.decode!("{\"name\":\"John\",\"age\":25}")
%{"name" => "John", "age" => 25}
```

## 詳しい解説：

JSONは 2001年に開発され、軽量でシンプルなデータフォーマットとして広く受け入れられてきました。他の代表的なフォーマットであるXMLに比べてパースや処理が高速であるため、データの交換や保存に利用されます。

Elixirでは、`Poison`や`Jason.so`などのモジュールもJSONの処理に使えるため、自分にとって最適な方法でJSONを操作することができます。

## 関連リンク：

- [Elixir 公式ウェブサイト](https://elixir-lang.org/)
- [JSON フォーマット仕様書](https://www.json.org/json-ja.html)
- [Poison モジュール](https://github.com/devinus/poison)