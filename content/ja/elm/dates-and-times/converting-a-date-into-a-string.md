---
date: 2024-01-20 17:36:29.363192-07:00
description: "How to: (\u3084\u308A\u65B9) Elm\u3067\u306F`Date`\u578B\u306F\u6642\
  \u523B\u3092\u7BA1\u7406\u3059\u308B\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u3067\u3059\
  \u3002`Date`\u30E2\u30B8\u30E5\u30FC\u30EB\u306F`Date`\u578B\u306E\u5024\u3092\u6587\
  \u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u95A2\u6570\u306A\u3069\u3001\u65E5\u4ED8\
  \u306B\u95A2\u9023\u3059\u308B\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\
  `Date`\u578B\u306F\u5185\u90E8\u7684\u306B\u306F POSIX \u30BF\u30A4\u30E0\u30B9\u30BF\
  \u30F3\u30D7\u3092\u30DF\u30EA\u79D2\u5358\u4F4D\u3067\u4FDD\u6301\u3057\u307E\u3059\
  \u3002ISO\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.908963-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Elm\u3067\u306F`Date`\u578B\u306F\u6642\u523B\u3092\
  \u7BA1\u7406\u3059\u308B\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002`Date`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u306F`Date`\u578B\u306E\u5024\u3092\u6587\u5B57\u5217\u306B\
  \u5909\u63DB\u3059\u308B\u95A2\u6570\u306A\u3069\u3001\u65E5\u4ED8\u306B\u95A2\u9023\
  \u3059\u308B\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002`Date`\u578B\u306F\
  \u5185\u90E8\u7684\u306B\u306F POSIX \u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3092\
  \u30DF\u30EA\u79D2\u5358\u4F4D\u3067\u4FDD\u6301\u3057\u307E\u3059\u3002ISO 8601\u5F62\
  \u5F0F\u306F\u56FD\u969B\u6A19\u6E96\u3068\u3057\u3066\u5E83\u304F\u4F7F\u308F\u308C\
  \u3066\u304A\u308A\u3001\u305D\u306E\u305F\u3081\u306E\u95A2\u6570\u3082\u8A00\u8A9E\
  \u306B\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\u4EE3\u308F\u308A\u306B\u30AB\
  \u30B9\u30BF\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u4F7F\u7528\u3057\u305F\
  \u3044\u5834\u5408\u306F\u3001`elm-time-format`\u306A\u3069\u306E\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30FC\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u63A2\u3059\
  \u3053\u3068\u306B\u306A\u308A\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
