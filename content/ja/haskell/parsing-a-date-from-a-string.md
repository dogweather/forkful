---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析するとは、文字列形式の日付をコンピュータが理解できる日付の形式に変換することです。プログラマがこれを行う理由は、文字列として取得した日付データをプログラム内で操作しやすくするためです。 

## 使い方：

Haskellでは、`Data.Time.Format`ライブラリを使って日付の解析を行います。以下にその例を示します：

```Haskell
import Data.Time 

stringToDate :: String -> IO Day 
stringToDate dateString = do
  let day = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString :: Maybe Day
  case day of
    Just d  -> return d
    Nothing -> error "invalid date format"

main = do
  d <- stringToDate "2020-12-31"
  print d
```

このプログラムは、"2020-12-31"という文字列を2020年12月31日という日付に解析するものです。出力は`2020-12-31`となります。

## 深堀り

Haskellの`Data.Time.Format`ライブラリは、1975年のICU(International Components for Unicode)プロジェクトから派生したものです。このライブラリは、プログラム内で日付や時刻を効果的に扱うための多くの機能を提供します。

日付の解析には他にも方法があり、例えば`Text.Parsec`ライブラリを使う方法もありますが、`Data.Time.Format`はその手軽さと簡便さからよく使用されます。

日付解析の具体的な実装には、日付形式のパターン一致と文字列操作が行われます。指定された形式に従って、文字列が適切に分割され、その各部分が対応する日、月、年に割り当てられます。

## 参考文献

以下に関連するリソースをいくつか紹介します：

- Haskellの公式ドキュメンテーション: [Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- Haskellでの純粋なパーサの作成: [Text.Parsec](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html)