---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:57.533680-07:00
description: "\u65B9\u6CD5 Haskell\u3067\u306F\u3001`cassava`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u3066CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u3053\u306E\
  \u76EE\u7684\u306E\u305F\u3081\u306E\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u3064\u3067\
  \u3059\u3002\u4EE5\u4E0B\u306F\u3001`cassava`\u3092\u4F7F\u7528\u3057\u3066CSV\u30D5\
  \u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\u8FBC\u307F\u3068\u66F8\u304D\u8FBC\u307F\
  \u3092\u884C\u3046\u4F8B\u3067\u3059\u3002 **1. CSV\u30D5\u30A1\u30A4\u30EB\u306E\
  \u8AAD\u307F\u8FBC\u307F\uFF1A**\u2026"
lastmod: '2024-03-13T22:44:42.217114-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067\u306F\u3001`cassava`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u3053\u306E\u76EE\u7684\u306E\
  \u305F\u3081\u306E\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u88FD\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u3064\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306F\u3001`cassava`\u3092\u4F7F\u7528\u3057\u3066CSV\u30D5\u30A1\u30A4\u30EB\
  \u304B\u3089\u8AAD\u307F\u8FBC\u307F\u3068\u66F8\u304D\u8FBC\u307F\u3092\u884C\u3046\
  \u4F8B\u3067\u3059."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法
Haskellでは、`cassava`ライブラリを使用してCSVファイルを扱うことができます。これは、この目的のための人気のあるサードパーティ製ライブラリの一つです。以下は、`cassava`を使用してCSVファイルから読み込みと書き込みを行う例です。

**1. CSVファイルの読み込み：**

まず、プロジェクトのcabalファイルに追加するか、Stackを使用して`cassava`がインストールされていることを確認します。

ここに、CSVファイルを読み込んで各レコードを出力する簡単な例を示します。CSVファイルにはnameとageの2つの列があると仮定します。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " is " ++ show (age :: Int) ++ " years old."
```

`people.csv`に以下が含まれていると仮定します：
```
John,30
Jane,25
```
出力は以下になります：
```
John is 30 years old.
Jane is 25 years old.
```

**2. CSVファイルの書き込み：**

CSVファイルを作成するには、`cassava`から`encode`関数を使用できます。

ここでは、レコードのリストをCSVファイルに書き込む方法を示します：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

このプログラムを実行した後、`output.csv`には以下が含まれます：

```
John,30
Jane,25
```

この`cassava`ライブラリを使用してHaskellでCSVファイルを扱うための簡潔な紹介は、CSVファイルからの読み取りと書き込みの両方を示しており、言語に慣れていない人にとってもデータ操作タスクをよりアプローチ可能にしています。
