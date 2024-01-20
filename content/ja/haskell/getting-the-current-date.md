---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何とその理由？

プログラミング言語Haskellで現在の日付を取得するとは、現在時刻から日付部分を抽出することを言います。これはログのタイムスタンプや日次レポートなど、日付情報が重要な場面で頻繁に行われます。

## 手順:

Haskellの標準ライブラリ`Data.Time`を使用して現在の日付を取得する基本的な方法を紹介します。

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    currentDay <- getCurrentTime >>= return . utctDay
    print currentDay
```

このコードを実行すると、現在の日付がYYYY-MM-DD形式で出力されます（例:2021-05-20）。

## ディープダイブ:

Haskellでは2001年のリリース以来、`Data.Time`パッケージが広く使用されています。他の選択肢としては古くから使われている`System.Time`ライブラリがありますが、現代のコードではほとんど使われていません。

Haskellの日付取得関数は、実装の詳細を抽象化し、システムの時刻を素早く簡単に取得できます。しかし、この情報はUTC（協定世界時）であり、特定のタイムゾーンに変換したい場合には追加の変換が必要となります。

## 関連情報:

より詳細な情報については以下のリンクを参照してください。

* [HaskellのData.Timeライブラリ](https://hackage.haskell.org/package/time)
* [タイムゾーン変換を伴う日付操作](https://stackoverflow.com/questions/13080592/whats-the-haskell-time-library-to-use)