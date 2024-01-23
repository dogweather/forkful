---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONデータの操作はよくあります。Elixir でのJSONは、データ交換が簡単で、APIやWebサービスとの通信に使われます。

## How to: (方法)
ElixirでのJSON扱いには、PoisonやJasonなどのライブラリが必要です。以下はJasonの使い方例です。

```Elixir
# JasonライブラリをMixに追加
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# JSON文字列をElixirのマップに変換
json_string = "{\"key\": \"value\"}"
map = Jason.decode!(json_string)
IO.inspect(map) # 出力: %{"key" => "value"}

# Elixirデータ構造をJSON文字列に変換
map = %{"key" => "value"}
json_string = Jason.encode!(map)
IO.puts(json_string) # 出力: {"key":"value"}
```

## Deep Dive (深掘り)
JSONはJavaScript Object Notationの略で、軽量なデータ交換フォーマットです。2001年に導入され、その使いやすさからXMLを置き換える主流のフォーマットになりました。Elixirでは、標準でJSONのサポートがありませんが、PoisonやJasonのようなライブラリを使うことで簡単に実装できます。これらのライブラリは、パフォーマンスや使いやすさにおいて異なる選択肢を提供します。

## See Also (関連情報)
- [Jason GitHubページ](https://github.com/michalmuskala/jason)
- [Elixirの公式ページ](https://elixir-lang.org)
- [JSONの公式サイト](https://www.json.org/json-ja.html)
