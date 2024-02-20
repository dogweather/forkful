---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:57.533680-07:00
description: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\
  \u3046\u4F5C\u696D\u306B\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u30C6\u30AD\u30B9\
  \u30C8\u30D9\u30FC\u30B9\u306E\u5F62\u5F0F\u3067\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\
  \u30BF\u3092\u4FDD\u5B58\u3059\u308B\u30D5\u30A1\u30A4\u30EB\u306E\u89E3\u6790\u3068\
  \u751F\u6210\u304C\u542B\u307E\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30B9\u30D7\u30EC\u30C3\u30C9\u30B7\u30FC\u30C8\u3001\u30C7\u30FC\
  \u30BF\u30D9\u30FC\u30B9\u304B\u3089\u306E\u30C7\u30FC\u30BF\u306E\u52B9\u7387\u7684\
  \u306A\u30A4\u30F3\u30DD\u30FC\u30C8\u3084\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\
  \u307E\u305F\u306F\u7570\u306A\u308B\u30D7\u30ED\u30B0\u30E9\u30E0\u9593\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3001\
  \u983B\u7E41\u306B\u3053\u306E\u4F5C\u696D\u306B\u5F93\u4E8B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.359241
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\u3046\
  \u4F5C\u696D\u306B\u306F\u3001\u30B7\u30F3\u30D7\u30EB\u3067\u30C6\u30AD\u30B9\u30C8\
  \u30D9\u30FC\u30B9\u306E\u5F62\u5F0F\u3067\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\
  \u3092\u4FDD\u5B58\u3059\u308B\u30D5\u30A1\u30A4\u30EB\u306E\u89E3\u6790\u3068\u751F\
  \u6210\u304C\u542B\u307E\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30B9\u30D7\u30EC\u30C3\u30C9\u30B7\u30FC\u30C8\u3001\u30C7\u30FC\u30BF\
  \u30D9\u30FC\u30B9\u304B\u3089\u306E\u30C7\u30FC\u30BF\u306E\u52B9\u7387\u7684\u306A\
  \u30A4\u30F3\u30DD\u30FC\u30C8\u3084\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u307E\
  \u305F\u306F\u7570\u306A\u308B\u30D7\u30ED\u30B0\u30E9\u30E0\u9593\u306E\u30C7\u30FC\
  \u30BF\u4EA4\u63DB\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3001\u983B\
  \u7E41\u306B\u3053\u306E\u4F5C\u696D\u306B\u5F93\u4E8B\u3057\u307E\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

CSV（カンマ区切り値）を扱う作業には、シンプルでテキストベースの形式で表形式のデータを保存するファイルの解析と生成が含まれます。プログラマーは、スプレッドシート、データベースからのデータの効率的なインポートやエクスポート、または異なるプログラム間のデータ交換を容易にするために、頻繁にこの作業に従事します。

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
