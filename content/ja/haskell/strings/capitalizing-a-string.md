---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:28.090850-07:00
description: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u3057\
  \u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\u3092\u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u5316\u3068\u3044\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u51FA\u529B\u306E\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u6574\u3048\u308B\u305F\u3081\u3001\u30C6\u30AD\
  \u30B9\u30C8\u306E\u6587\u6CD5\u7684\u6B63\u78BA\u3055\u3092\u5B88\u308B\u305F\u3081\
  \u3001\u307E\u305F\u306F\u751F\u6210\u3055\u308C\u305F\u30C7\u30FC\u30BF\u306E\u8AAD\
  \u307F\u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.158579-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u3057\
  \u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\u3092\u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u5316\u3068\u3044\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u51FA\u529B\u306E\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u6574\u3048\u308B\u305F\u3081\u3001\u30C6\u30AD\
  \u30B9\u30C8\u306E\u6587\u6CD5\u7684\u6B63\u78BA\u3055\u3092\u5B88\u308B\u305F\u3081\
  \u3001\u307E\u305F\u306F\u751F\u6210\u3055\u308C\u305F\u30C7\u30FC\u30BF\u306E\u8AAD\
  \u307F\u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## What & Why?
文字列の先頭を大文字にし、残りの文字を小文字にすることを文字列の大文字化といいます。プログラマーは、出力のフォーマットを整えるため、テキストの文法的正確さを守るため、または生成されたデータの読みやすさを向上させるためにこれを行います。

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
