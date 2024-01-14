---
title:    "Elixir: パターンに合致する文字を削除する"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

ある特定のパターンにマッチする文字を削除することのイディオムを理解することは、Elixirプログラミングにおける重要なスキルです。これを行うことで、より簡潔なコードを書くことができ、パフォーマンスを最適化することができます。

## 方法

文字列やリストから特定のパターンにマッチする文字を削除するには、Elixirの標準モジュールであるRegexを使用します。下記のコード例を参考にしてください。

```Elixir
str = "Hello, world!"
regex = ~r/[aeiou]/ # 母音を表す正規表現パターン
result = str |> Regex.replace(regex, "") # 結果は"Hll, wrld!"になります
```

または、リストの場合は下記のように書くことができます。

```Elixir
list = [1, 2, 3, 4, 5]
regex = ~r/[24680]/ # 偶数を表す正規表現パターン
result = list |> Enum.filter(fn x -> not Regex.match?(regex, to_string(x)) end) # 結果は[1, 3, 5]になります
```

## 深堀り

Regexモジュールを使用することで、一度に複数の文字を置換することもできます。また、正規表現のメタキャラクタやオプションを使うことで、より複雑なパターンにもマッチさせることができます。詳細についてはElixirの公式ドキュメントを参照してください。

## 参考リンク

- [ElixirのRegexモジュール](https://hexdocs.pm/elixir/Regex.html)
- [正規表現の基礎](https://qiita.com/jnchito/items/893c887fbf19e17ee464)
- [Elixirで正規表現を使う方法](https://qiita.com/hmuronaka/items/708f4f4d9b7157c76521) 

## 参考

[正規表現のパターンにマッチする文字を削除する方法](https://exampleblog.com/)