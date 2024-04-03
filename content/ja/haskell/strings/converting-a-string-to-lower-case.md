---
date: 2024-01-20 17:38:28.833084-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.164011-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
