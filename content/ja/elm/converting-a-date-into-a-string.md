---
title:                "日付を文字列に変換する"
aliases:
- ja/elm/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:29.363192-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するとは、日付のデータ型を人が読めるテキスト形式にすることです。ログやユーザーインターフェース表示のために行います。

## How to: (やり方)
```Elm
import Time
import Task
import Date exposing (Date)

-- 日付を文字列に変換する関数
formatDate : Date -> String
formatDate date =
    let
        -- ISO 8601 形式の例 "2023-01-30"
        isoString = Date.toIsoString date
    in
    isoString

-- サンプル使用例
example : Task x String
example =
    Time.now
        |> Task.map (\currentTime -> formatDate (Date.fromPosix currentTime))

-- ターミナルに出力して確認するための未遂的な関数
tryFormatting : Task x String -> Task x ()
tryFormatting task =
    task |> Task.map (Debug.log "Formatted date")

-- 実行
example
    |> tryFormatting
```

出力例：
```
"Formatted date: 2023-01-30"
```
## Deep Dive (探求)
Elmでは`Date`型は時刻を管理する一般的な方法です。`Date`モジュールは`Date`型の値を文字列に変換する関数など、日付に関連する機能を提供します。`Date`型は内部的には POSIX タイムスタンプをミリ秒単位で保持します。ISO 8601形式は国際標準として広く使われており、そのための関数も言語に含まれています。代わりにカスタムフォーマットを使用したい場合は、`elm-time-format`などのサードパーティーのパッケージを探すことになります。

## See Also (関連情報)
- Elmの公式`Date`ドキュメント: https://package.elm-lang.org/packages/elm/time/latest/Date
- POSIX タイムについて詳しく: https://en.wikipedia.org/wiki/Unix_time
- ISO 8601 標準に関する情報: https://www.iso.org/iso-8601-date-and-time-format.html
- カスタム日付フォーマットパッケージ`elm-time-format`: https://package.elm-lang.org/packages/ryannhg/elm-time-format/latest/
