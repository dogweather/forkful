---
title:                "正規表現を使用する"
html_title:           "Elixir: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ使うのか？

 正規表現を使う理由は、パターンに基づいたテキストマッチングやパターン置換が必要な時に便利だからです。

## 使い方

正規表現を使うには、`Regex`モジュールを使用します。例えば、テキストから数字のみを抽出するには以下のように記述します。

```Elixir
Regex.run(~r/\d+/, "abc123def456") # => ["123", "456"]
```

また、正規表現を使って文字列の置換もできます。例えば、エラーを含む文字列からエラーメッセージのみを抽出するには以下のように記述します。

```Elixir
Regex.replace(~r/Error: (.+)/, "Error: Something went wrong.", "\\1") # => "Something went wrong."
```

## 実践的な使い方

正規表現をより深く理解するために、パターンマッチングやキャプチャーグループなどのさまざまな機能を詳しく学びましょう。また、`Regex`モジュールのドキュメントを参照することもお勧めします。

## 併せて読みたい

- [Elixir Regex ドキュメント](https://hexdocs.pm/elixir/Regex.html)
- [正規表現チュートリアル](https://regexone.com/) (英語)