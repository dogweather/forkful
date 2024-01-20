---
title:                "文字列を大文字にする"
html_title:           "Elm: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列を大文字化するとは文字列内のあらゆる小文字文字を対応する大文字に変換するという操作です。プログラマーはこれを主に、一貫性を保ち使用者が誤りやすい小大文字の混在を避けるために行います。

## どうするのか:

以下、Elmでの文字列大文字化のコード例とその出力結果です。

```Elm
import String

capitalize : String -> String
capitalize str = 
    String.toUpper str

main = 
    print (capitalize "hello world!")
```

このコードを実行すると、"HELLO WORLD!"と大文字化された文字列が出力されます。

## 深堀り:

1. **歴史的背景**
大文字と小文字の概念は古代ローマから存在しています。しかし、コンピュータプログラミングにおける文字列の大文字化は、主にコンピュータが主流になった1970年代以降にクロスプラットフォーム互換性や一貫性を保つために使われてきました。

2. **代替手段**
大文字化のみならず、Elmには小文字化(`String.toLower`)や先頭のみ大文字化(`String.capitalize`)といった関数もあります。

3. **実装の詳細**
Elmの`String.toUpper`関数はUTF-16互換の全ての文字を対応する大文字に置換します。特定の文字が大文字バージョンを持たない場合、元の文字はそのまま残されます。

## 関連情報:

- Elmの文字列操作については、公式ドキュメンテーションで詳しく解説されています: [Elm String Module](https://package.elm-lang.org/packages/elm/core/latest/String) 
- Unicodeについてのより詳しい情報はこちらで提供されています: [Unicode.org](https://www.unicode.org) 