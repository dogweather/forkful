---
title:    "Elm: パターンに一致する文字の削除"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

「あるパターンにマッチする文字を削除する」という作業を行う理由は、テキストを処理する際に特定の文字を除外する必要があるからです。

## 方法

まず、Elmの文字列操作機能を利用して、与えられた文字列から特定の文字パターンを検索し、その文字を削除する関数を作成する必要があります。

例えば、以下のようなコードを使用して特定の文字パターンを削除することができます。

```Elm
deletePattern : String -> String -> String
deletePattern pattern string =
    String.replace pattern "" string
```

そして、以下のように呼び出すことで、特定の文字パターンを削除した結果が得られます。

```Elm
deletePattern "a" "banana" -- => "bnana"
```

## ディープダイブ

文字列操作を行う際には、様々な文字パターンを検索して削除することができます。例えば、正規表現を使用することで、複雑なパターンにもマッチする文字を削除することができます。

また、Stringモジュールには、さまざまなメソッドが用意されており、より効率的に文字列操作を行うことができます。詳細については、公式ドキュメントを参照してください。

## 参考リンク

[Elm公式ドキュメント](https://guide.elm-lang.org/)  
[正規表現を使用する場合の例](https://discourse.elm-lang.org/t/how-to-deleting-characters-matching-a-pattern/4633/2)  
[stringモジュールの詳細](https://package.elm-lang.org/packages/elm/core/1.0.3/String)