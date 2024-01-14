---
title:                "Elixir: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字列のパターンにマッチする文字を削除することの重要性を説明します。この技術により、データのクリーニングや整形を効率的に行うことができます。

## 方法

以下のようなコードブロックを使いながら、削除の方法を説明します。

```Elixir
str = "Hello, world! Hello, Elixir!" # 入力文字列
regex = ~r/Elixir/ # マッチするパターン
output = String.replace(str, regex, "") # マッチした文字を削除
IO.puts output # 出力結果: Hello, world! Hello, !
```

このように、Elixirでは`String.replace/3`関数を使って、文字列から指定したパターンにマッチする文字を削除することができます。

## ディープダイブ

文字列を扱うには、Elixirの`String`モジュールを使用します。しかし、正規表現を利用する場合は、`Regex`モジュールを組み合わせて使用することができます。

さらに、`String.gsub/3`関数を使用することで、パターンにマッチする全ての文字を削除することも可能です。

```Elixir
str = "Hello, world! Hello, Elixir!" # 入力文字列
regex = ~r/Hello/i # 大文字小文字を無視してマッチさせるパターン
output = String.gsub(str, regex, "") # マッチした文字を全て削除
IO.puts output # 出力結果: , world! , !
```

これにより、さらに柔軟な文字の削除ができるようになります。

## 参考リンク

- [Elixirで文字列を扱う方法 (公式ドキュメント)](https://hexdocs.pm/elixir/String.html)
- [正規表現を使って文字列を操作する方法 (公式ドキュメント)](https://hexdocs.pm/elixir/Regex.html)
- [Elixirで文字列を置換する方法 (Qiita)](https://qiita.com/syou007/items/b32c2f3408b6399b2579)
- [正規表現の基礎知識 (TechAcademy Magazine)](https://techacademy.jp/magazine/15344)

## 関連リンク

- [Elixirの基本構文を学ぶ (Casual Code)](https://casualcode.net/elixir-basic-syntax-japanese/)
- [Elixirにおけるパイプライン演算子の活用法 (Medium)](https://medium.com/codefellows/elixir-enumerables-and-pipelines-8c9931fd0a21)