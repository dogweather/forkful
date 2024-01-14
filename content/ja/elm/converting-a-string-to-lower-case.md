---
title:                "Elm: 「文字列を小文字に変換する」"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

Stringを小文字に変換することに関心があるかもしれません。例えば、入力されたユーザー名やパスワードを小文字に変換して、大文字と小文字を区別せずに認証する必要がある場合があります。また、文字列を比較する際に、大文字と小文字を区別せずに処理したい場合もあります。

## 方法

文字列を小文字に変換するには、Stringモジュールの`toLower`関数を使用します。下記の例では、文字列 "Elm Programming" を小文字に変換し、コンソールに出力しています。

```Elm
import String exposing (toLower)

main =
    let
        inputString = "Elm Programming"
        convertedString = toLower inputString
    in
        convertedString
```

上記のコードを実行すると、"elm programming"という出力が得られます。文字列が小文字に変換されたことが確認できます。

## 深堀り

Stringモジュールには、文字列を大文字・小文字を無視して比較するための`compareIgnoreCase`や、特定の文字列を検索してその位置を返す`indexes`といった便利な関数があります。また、文字列操作以外にも、日付や数値の操作など、さまざまな便利な関数が含まれています。詳細は公式ドキュメントを参照してください。

## See Also
- Elm公式ドキュメント: https://guide.elm-lang.org/
- Stringモジュールのドキュメント: https://package.elm-lang.org/packages/elm/core/latest/String