---
title:                "Elixir: 正規表現の使用"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか？

正規表現は、テキストデータを処理する際に非常に便利です。例えば、特定のパターンを持つ文字列を検索したり、置換したりすることができます。また、文章の自然言語処理やデータの検証などにも活用できます。

## 正規表現の使い方

正規表現はElixirの標準ライブラリに含まれており、```Regex```モジュールを使用して利用することができます。以下のコード例では、文字列から特定のパターンを持つ単語を抽出しています。

```Elixir
string = "私の名前はジョンです"
Regex.scan(~r/私の名前は(\w+)/u, string) |> List.first |> List.last
```

上記のコードの実行結果は、```ジョン```という文字列になります。

## 深堀りする

正規表現の表記法や特殊文字の扱いなど、さらに詳細な情報を知りたい場合は、正規表現のドキュメントやチュートリアルを参考にすることができます。また、正規表現を使用する際に気をつけるべき点として、パフォーマンスの問題があります。大きなテキストデータを処理する際は、正規表現の最適化を行う必要があります。

## 参考

- [Elixir Regexモジュールのドキュメント](https://hexdocs.pm/elixir/Regex.html)
- [正規表現チュートリアル (英語)](https://www.regular-expressions.info/tutorial.html)
- [正規表現最適化の方法 (英語)](https://www.regular-expressions.info/optimization.html)

## その他のリソース

- [Elixir公式サイト](https://elixir-lang.org/)
- [Elixirチュートリアル (日本語)](https://elixir-ja.sena-net.works/guide/basic_types.html)
- [正規表現の基礎 (日本語)](https://www.slideshare.net/zensh75/61-japanese-tutorial)