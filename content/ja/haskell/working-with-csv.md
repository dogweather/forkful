---
title:                "CSVとの作業"
aliases:
- ja/haskell/working-with-csv.md
date:                  2024-02-03T19:19:57.533680-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSVとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
