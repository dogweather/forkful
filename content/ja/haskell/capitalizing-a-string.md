---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を大文字にするっていうのは、文字列の中の全ての小文字を大文字に変換することです。プログラマーは、ユーザーの入力を統一したり、コード内で文字列の扱いを規則正しくするために、この操作を行います。

## How to: (やり方)
Haskellでは、`Data.Char` モジュールの `toUpper` 関数を使って一文字ずつ大文字に変えることができます。ここに簡単な例を示します。

```haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

main :: IO ()
main = putStrLn (capitalize "hello, world")
```

実行結果は、次のようになります。

```
HELLO, WORLD
```

大文字にする操作は意外と簡単です。

## Deep Dive (深掘り)
文字列を大文字に変換する機能は、初期のプログラミング言語から存在しています。これには、データを並べ替えたり検索しやすくするといった利点があるためです。Haskellでは、`toUpper`は単一の文字を大文字にするためのものであり、`map`関数を用いてこれを文字列全体に適用します。他の方法として、`Data.Text`を使用することもありますが、多くの場合、`String`を用いるのが簡単です。

`toUpper` 関数自体はUnicodeに対応しており、さまざまな言語の文字にも適用可能です。ただし、全ての状況で期待する結果が得られるとは限らないため、具体的なケースに依存する場合があります。

例えば、ドイツ語には 'ß' という小文字専用の文字がありますが、これには大文字の形がありません。このような特殊なケースを扱うときはさらに注意が必要です。また、全ての文字が大文字を持っているわけでもないため、一部の記号や絵文字などはそのままになります。

## See Also (関連情報)
- `Data.Char` モジュールのドキュメント: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- Haskellにおける`String`型の操作方法: https://www.haskell.org/tutorial/strings.html
- UnicodeとHaskell: https://wiki.haskell.org/Unicode_input

Haskellでの文字列処理は、これらのリソースでさらに詳しく知ることができます。