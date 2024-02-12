---
title:                "文字列を小文字に変換"
aliases: - /ja/haskell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:28.833084-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列を小文字に変換するとは、全てのアルファベット文字を小文字版に置き換えることです。検索やソートの前に使用して文字列の大文字・小文字の違いを無視し、一貫性を保ちます。

## How to: (方法：)

```Haskell
import Data.Char (toLower)

-- 文字を小文字に変換
lowercaseChar :: Char -> Char
lowercaseChar c = toLower c

-- 文字列を小文字に変換
lowercaseStr :: String -> String
lowercaseStr = map toLower

-- 使い方
main :: IO ()
main = do
  let originalStr = "Haskell is Awesome!"
  let lowerStr = lowercaseStr originalStr
  putStrLn lowerStr
```

出力:

```
haskell is awesome!
```

## Deep Dive (深堀り)

### 歴史的背景：
大文字と小文字の区別は、文字が打ち出された印刷術の発展に由来します。Haskellの `Data.Char` モジュール（`toLower`関数を含む）は、Unicode 標準に準拠し、多様な文字コードに対応しています。

### 代替案：
他の方法として `Data.Text.toLower`（`text` パッケージから）や自作変換関数を使うことがありますが、 `map` と `Data.Char.toLower` の組み合わせは簡単で直感的です。

### 実装の詳細：
`toLower` はUnicodeの大文字と小文字の対応を考えて設計されているので、英文字だけではなく世界中の言語の文字に適用可能です。

## See Also (関連情報)

- Haskell `Data.Char` モジュール: [Hackage Data.Char](https://hackage.haskell.org/package/base-4.16.2.0/docs/Data-Char.html)
- Unicode 標準に関する情報: [Unicode Consortium](https://www.unicode.org)
- `text` パッケージ: [Hackage Data.Text](https://hackage.haskell.org/package/text)
