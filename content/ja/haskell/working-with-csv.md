---
title:                "csvを扱う"
html_title:           "Haskell: csvを扱う"
simple_title:         "csvを扱う"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

CSVとは、コンマで区切られたデータを表すファイル形式です。プログラマーは、データセットを読み込んだり、処理したりするために、CSVファイルを扱うことがあります。

## 方法:

### CSVファイルを読み込む

まず、`haskell-csv`ライブラリをインストールします。次に、ファイルからデータを読み込みます。

```haskell
import Text.CSV

-- CSVファイルを読み込む関数
readCSV :: FilePath -> IO CSV
readCSV path = do
  csv <- parseCSVFromFile path
  case csv of
    Left err -> error $ "エラー: " ++ show err
    Right res -> return res
```
 
### CSVファイルを書き込む

新しいCSVファイルを作成し、データを書き込むこともできます。

```haskell
-- CSVファイルを書き込む関数
writeCSV :: FilePath -> CSV -> IO ()
writeCSV path csv = writeFile path $ printCSV csv
```

### CSVデータを処理する

読み込んだCSVデータを処理することもできます。例えば、2列目のデータを合計する関数を作成すると、次のようになります。

```haskell
-- 2列目のデータを合計する関数
sumCol2 :: CSV -> Int
sumCol2 csv = sum $ map (\row -> read (row!!1) :: Int) $ tail csv
```

## 詳細を調べる

### CSVの歴史的背景

CSVは、1970年代にプログラマーの間で普及し始めました。当時は、生データを構造化するのに便利なフォーマットとして認識されていました。

### 代替手段

CSV以外にも、データを表現するファイル形式はあります。例えば、TSV（タブ区切り）やJSONなどがあります。各フォーマットにはメリット・デメリットがありますので、プロジェクトの要件に合わせて選択することが重要です。

### 実装の詳細

`haskell-csv`ライブラリは、HaskellでCSVファイルを扱う際に便利な関数を提供します。詳細なドキュメントは、[Hackage](https://hackage.haskell.org/package/csv)で確認できます。

## 関連情報を見る

- [Haskell入門-はじめてのプログラミング言語-](https://www.amazon.co.jp/dp/B07YF3C1CQ/ref=dp-kindle-redirect?_encoding=UTF8&btkr=1)：Haskellの基本的な概念を学べる本です。
- [Haskell Wiki](https://wiki.haskell.org/CSV)：HaskellでCSVを扱うための解説やリンクがまとめられています。