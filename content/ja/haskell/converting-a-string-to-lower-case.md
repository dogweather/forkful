---
title:                "Haskell: 文字列を小文字に変換する"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することについては、実生活でよくあるシナリオを想像できます。例えば、ユーザーがフォームに入力した文字列を小文字に変換して、データベースに保存したい場合などがあります。これは、より効率的な検索や比較が可能になるため、多くの場面で必要とされるタスクです。

## 方法

まずは、```Haskell```コードブロック内における基本的な方法を紹介します。

```Haskell
import Data.Char

-- 文字列を小文字に変換する関数
toLowerString :: String -> String
toLowerString s = map toLower s
```

上記のコードは、文字列を受け取って、それぞれの文字を小文字に変換する```toLower```関数を利用しています。そして、```map```関数を使って、文字列全体に対してこの処理を適用させています。実際に試してみると、以下のような結果になります。

```Haskell
toLowerString "HELLO"  -- 出力: "hello"
```

また、より一般的な文字列操作のライブラリである```Data.Text```を使っても同じことができます。

```Haskell
import Data.Text (toLower, pack, unpack)

-- 文字列を小文字に変換する関数
toLowerString :: String -> String
toLowerString s = unpack $ toLower $ pack s
```

上記の例では、まず文字列を```pack```関数で```Text```型に変換し、```toLower```関数を適用させてから、再び```unpack```関数を使って文字列型に戻しています。これも同じ結果になります。

```Haskell
toLowerString "HELLO"  -- 出力: "hello"
```

## ディープダイブ

文字列を小文字に変換する方法は、データ型の一つである```Char```型を扱うことによって実現されています。```Data.Char```モジュールには、様々な文字操作を行うための便利な関数が用意されています。そこで、ぜひ一度ドキュメントを読んでみることをお勧めします。

また、もしこれ以上の文字列操作を学びたい場合は、```Data.Text```モジュールを用いることでより高度な処理を行うことができます。例えば、文字列パターンの検索や置換、文字列の比較などが可能になります。こちらもぜひ調べてみてください。

## 関連情報

- [Haskell - Data.Char Documentation](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskell - Data.Text Documentation](https://hackage.haskell.org/package/text/docs/Data-Text.html)