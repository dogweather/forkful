---
title:    "Gleam: 正規表現を使用する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

Gleamプログラミングブログへようこそ！

## Why

正規表現を使用する理由は何でしょうか？正規表現を使用すると、文字列内で特定のパターンを検索したり置換したりすることができます。これはプログラミングにおいて非常に便利であり、データの処理やバリデーションに役立ちます。

## How To

正規表現を使用するためには、まずは文字列を表す正規表現パターンを作成する必要があります。例えば、文字列内で特定の文字列を検索する場合は、その文字列を正規表現パターンとして指定します。

```Gleam
let pattern = regex.new("hello")
```

次に、この正規表現パターンを実際の文字列と照合するために、`match`関数を使用します。例えば、文字列"hello world"に対して`match`関数を実行すると、この文字列に"hello"という部分が含まれているかどうかを判定することができます。

```Gleam
match("hello world", pattern) // true
```

さらに、正規表現パターンを使用して文字列内のパターンを置換することもできます。例えば、"hello world"内の"hello"を"こんにちは"に置換する場合は、`replace`関数を使用します。

```Gleam
regex.replace("hello world", pattern, "こんにちは") // こんにちは world
```

## Deep Dive

正規表現にはさまざまなオプションや特殊な記号があり、より詳細に制御してパターンを作成することができます。例えば、文字の一部のみを置換したり、大文字と小文字を区別しないようにしたりすることができます。

詳しくは、Gleamの公式ドキュメントや正規表現のチュートリアルを参照してください。

## See Also

- Gleam正規表現チュートリアル
- Gleam公式ドキュメント
- 正規表現の基礎知識を学ぶためのチュートリアル