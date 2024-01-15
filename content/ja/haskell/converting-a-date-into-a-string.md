---
title:                "日付を文字列に変換する"
html_title:           "Haskell: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜHaskellで日付を文字列に変換するのか

日付を文字列に変換することは、プログラミングにおいて非常に一般的な作業です。Haskellは強力な型システムを備えているため、日付を正確に表現し、安全に操作することができます。また、Haskellの関数型プログラミングスタイルは、日付の変換をより簡潔で読みやすいコードにすることができます。

## 方法：日付を文字列に変換するためのコーディング例

日付を文字列に変換するには、Haskellの「Data.Time.Format」モジュールを使用します。以下のコードブロックには、日付を特定の書式にフォーマットする方法が示されています。

```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)

-- 現在の日付を"YYYY/MM/DD"形式にフォーマットする
getCurrentDate :: IO String
getCurrentDate = do
  currentTime <- getCurrentTime -- 現在の時刻を取得
  return $ formatTime defaultTimeLocale "%Y/%m/%d" currentTime
```

上記のコードを実行すると、現在の日付が"2021/05/02"のようにフォーマットされた文字列として返されます。

また、日付を任意の書式にフォーマットするカスタム関数を作成することもできます。以下の例では、日付を"MM/DD/YYYY"形式にフォーマットする関数を定義しています。

```Haskell
formatDate :: Day -> String
formatDate date = formatTime defaultTimeLocale "%m/%d/%Y" date

-- "2021-05-02"を"05/02/2021"にフォーマット
print $ formatDate $ read "2021-05-02" :: IO String
```

実行すると、"05/02/2021"が出力されます。

## ディープダイブ：日付を文字列に変換する際の注意点

日付を文字列に変換する際には、文字列として扱いたい日付を「Day」または「UTCTime」のような日付型に変換することが必要です。また、フォーマットに使用するパターンも、簡単に入力ミスを起こしてしまうことがあるので注意が必要です。

さらに、Haskellは静的型付け言語のため、日付の変換に伴うエラーをコンパイル時に発見することができます。これにより、実行時に予期せぬエラーが発生する可能性を低くすることができます。

## その他の参考リンク

- [Haskell 公式ドキュメント](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html)
- [Hackage - Haskellのパッケージデータベース](https://hackage.haskell.org/)
- [Real World Haskell - 実践的なHaskellの学習サイト](http://book.realworldhaskell.org/read/)
- [Haskell Cafe - Haskellに関するディスカッションフォーラム](https://groups.io/g/haskell-cafe)
- [Haskell.jp - 日本語コミュニティサイト](https://haskell.jp/)