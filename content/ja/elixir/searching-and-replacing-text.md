---
title:                "Elixir: テキストの検索と置換"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ
なぜテキストの検索や置換を行うのかを説明します。テキストの置換を行うことで、複数のファイルや文書の単語やフレーズを一度に変更することができ、作業効率が向上します。

## 方法
検索や置換を行うためのElixirのコーディング例やサンプル出力を以下のコードブロックに示します。

```Elixir
# 文字列の検索と置換
string = "こんにちは世界"
search_term = "世界"
replacement = "日本"
new_string = String.replace(string, search_term, replacement)

# 出力: "こんにちは日本"
```

また、正規表現を使用して複雑な検索や置換を行うこともできます。

```Elixir
# 正規表現による検索と置換
string = "テキスト123456"
search_term = ~r/\d+/
replacement = "数字"
new_string = String.replace(string, search_term, replacement)

# 出力: "テキスト数字"
```

## ディープダイブ
テキストの検索や置換を行う際、複数のオプションやパラメータを利用することができます。これらのオプションやパラメータを詳しく説明します。

- `global: true`オプションを使用することで、全てのマッチした文字列を置換します。
- `replace_repetitions: true`オプションを使用することで、同じ文字列が連続している場合に一度の置換でまとめて置換されます。
- `count`パラメータを使用することで、置換を行う回数を制限することができます。

詳細は公式ドキュメントを参照してください。

## See Also
- 公式ドキュメント: https://hexdocs.pm/elixir/String.html#replace/4
- 正規表現チュートリアル: https://qiita.com/jnchito/items/9a00f01807c77dec894c

ありがとうございました。Elixirの検索と置換を習得することで、より効率的なコーディングが可能になります。