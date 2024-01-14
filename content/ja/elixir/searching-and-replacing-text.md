---
title:    "Elixir: テキストを検索して置換する"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

文章の検索や置換をする理由は様々ありますが、Elixirであればパターンマッチングを使用することで、より高速かつ柔軟なテキストの検索と置換が可能です。

## 方法

まずは、Elixirのパターンマッチングを使用して文字列の中から特定のパターンを検索する方法を見ていきましょう。以下のコードを見てください。

```
Elixir case "Elixir is a powerful and functional programming language" do 
  {String.contains?("Elixir"), "Found Elixir!"} 
  {_, "Elixir not found"} 
end
```

このコードでは、`String.contains?`という関数を使用して、「Elixir」というパターンを検索しています。もし文字列内に「Elixir」が含まれている場合、`{String.contains?("Elixir"), "Found Elixir!"}`のパターンにマッチし、「Found Elixir!」というメッセージが返されます。もし文字列内に「Elixir」が含まれていない場合、`{_, "Elixir not found"}`のパターンにマッチし、「Elixir not found」というメッセージが返されます。

次に、パターンにマッチした文字列を置換する方法を見てみましょう。以下のコードを見てください。

```
Elixir "Elixir is a powerful and functional programming language" 
|> String.replace("Elixir", "Ruby")
```

このコードでは、`String.replace`という関数を使用して「Elixir」という文字列を「Ruby」という文字列に置換しています。コンソールには「Ruby is a powerful and functional programming language」という文字列が出力されます。

## 深堀

Elixirでのテキストの検索と置換では、パターンマッチングの他にも様々な方法があります。例えば、正規表現を使用することでより柔軟な検索を行うことができます。また、`Regex`モジュールを使用することでさらに高度なテキスト処理を行うことも可能です。

さらに、Elixirはコンパイル時に構文解析を行うため、処理速度が非常に高速です。これにより、大量のテキストを効率的に処理することができます。

## 参考文献

- [Elixir 公式ドキュメント - String](https://hexdocs.pm/elixir/String.html)
- [Elixir 公式ドキュメント - Regex](https://hexdocs.pm/elixir/Regex.html)
- [Elixir 公式ドキュメント - パターンマッチング](https://elixir-lang.org/getting-started/pattern-matching.html)

## 関連リンク

- [Elixir: パターンマッチング入門](https://dev.to/ryuujo/elixir-how-to-use-pattern-matching-11kf)
- [Elixirで正規表現を使いこなす](https://qiita.com/torudai/items/cd877b8fd9e8070a6a5d)
- [Elixirにおける文字列操作の際のパフォーマンス改善方法](https://qiita.com/shuma/items/dab8a1bd6b4adb5c0381)