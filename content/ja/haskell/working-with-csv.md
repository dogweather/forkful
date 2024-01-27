---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？
CSVは「Comma-Separated Values」の略。テキストデータを保存・交換する簡単な形式。プログラマーはデータを編集、分析、保存のために使う。

## How to:
## 実践方法：
```Haskell
import qualified Data.Csv as Csv
import Data.ByteString.Lazy as BL
import Data.Vector as V

-- CSVファイルの読み込みとパーシング
main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case Csv.decode Csv.HasHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (name, age) ->
                   putStrLn $ name ++ " is " ++ show age ++ " years old."

-- CSVデータ
-- name,age
-- Alice,30
-- Bob,25
```
サンプル出力：
```
Alice is 30 years old.
Bob is 25 years old.
```

## Deep Dive
## 徹底解説：
CSVは1972年にIBMによって導入された。Excelやデータベースとの互換性のために今でも使われる。Haskellでは、`cassava`ライブラリが標準的。他に`pandoc`, `csv-conduit`などの代替品も。`cassava`は型安全かつ効率的にCSVデータを処理。

## See Also
## 参考情報：
- Cassava ライブラリのドキュメント: [https://hackage.haskell.org/package/cassava](https://hackage.haskell.org/package/cassava)
- HaskellのCSV処理に関するブログポスト: [https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/)
- CSVデータのより高度な処理のためのconduitライブラリ: [https://hackage.haskell.org/package/csv-conduit](https://hackage.haskell.org/package/csv-conduit)
