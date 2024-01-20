---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 比較的価値: Haskellで二つの日付を比較する方法 

## 何でしょうか？& なぜですか？
日付の比較とは、プログラムの一部として二つの日付を比較することです。この技術は、特定のイベントが他のイベントより前に発生したか、あるいはそれらの間に何日間が経過したかを知るために行われます。

## どのように？
以下に簡単な例を示します：  

``` Haskell
import Data.Time

main = do
    let date1 = fromGregorian 2020 1 1
    let date2 = fromGregorian 2020 1 31
    print (diffDays date2 date1)
```
出力： 

``` Haskell
30 
```

## ディープダイブ
Haskellでは、日付の操作は "Data.Time" モジュールを使用して行われます。このモジュールの歴史は古く、Haskell の初期から存在しています。なお、日付の比較の代替手段として `UTCTime` や `ZonedTime` を使用することも可能です。

## その他の参考文献
- [Haskellの公式ドキュメンテーション](https://www.haskell.org/documentation/)