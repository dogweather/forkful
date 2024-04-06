---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:28.090850-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u3067\u306F\
  \u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066\u3001\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\
  \u8981\u3068\u305B\u305A\u306B\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.699895-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u3067\u306F\u3001\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3063\u3066\u3001\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\
  \u3068\u305B\u305A\u306B\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どのようにして：
Haskellでは、標準ライブラリを使って、サードパーティのライブラリを必要とせずに文字列を大文字化することができます。

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- サンプル使用法:
main = putStrLn $ capitalize "hello world"
```

出力:
```
Hello world
```

より複雑なシナリオや使いやすさを求める場合は、Haskellで効率的な文字列操作に人気のある`text`というサードパーティライブラリの使用を検討したいかもしれません。

まず、プロジェクトの依存関係に`text`を追加する必要があります。その後、以下のようにしてその機能を使用して文字列を大文字化します：

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- textライブラリを使用したサンプル使用法:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

出力:
```
Hello world
```

これらの例は、サードパーティライブラリを使用するかどうかにかかわらず、Haskellで文字列を大文字化するためのシンプルで効果的な方法を示しています。
