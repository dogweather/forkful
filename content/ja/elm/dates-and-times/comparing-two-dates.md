---
date: 2024-01-20 17:33:14.408444-07:00
description: "\u6BD4\u8F03\u3063\u3066\u4F55\uFF1F\u305D\u308C\u306F\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u304A\u3044\u3066\u3001\u4E8C\u3064\u306E\u65E5\u4ED8\u304C\u3069\
  \u3046\u95A2\u9023\u3057\u3066\u308B\u304B\u3092\u78BA\u304B\u3081\u308B\u3053\u3068\
  \u3060\u3002\u306A\u305C\u3084\u308B\u306E\u304B\uFF1F\u30A4\u30D9\u30F3\u30C8\u304C\
  \u3044\u3064\u8D77\u3053\u308B\u306E\u304B\u3092\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\
  \u3057\u305F\u308A\u3001\u671F\u9650\u3092\u76E3\u8996\u3057\u305F\u308A\u3059\u308B\
  \u305F\u3081\u3055\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.052719-07:00'
model: gpt-4-1106-preview
summary: "\u6BD4\u8F03\u3063\u3066\u4F55\uFF1F\u305D\u308C\u306F\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u304A\u3044\u3066\u3001\u4E8C\u3064\u306E\u65E5\u4ED8\u304C\u3069\
  \u3046\u95A2\u9023\u3057\u3066\u308B\u304B\u3092\u78BA\u304B\u3081\u308B\u3053\u3068\
  \u3060\u3002\u306A\u305C\u3084\u308B\u306E\u304B\uFF1F\u30A4\u30D9\u30F3\u30C8\u304C\
  \u3044\u3064\u8D77\u3053\u308B\u306E\u304B\u3092\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\
  \u3057\u305F\u308A\u3001\u671F\u9650\u3092\u76E3\u8996\u3057\u305F\u308A\u3059\u308B\
  \u305F\u3081\u3055\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why?
比較って何？それはプログラムにおいて、二つの日付がどう関連してるかを確かめることだ。なぜやるのか？イベントがいつ起こるのかをスケジュールしたり、期限を監視したりするためさ。

## How to:
Elmで日付を比較する方法を見てみよう。ちゃちゃっとコード書いて、実際に動かす所までやってみよう。

```Elm
import Time exposing (Posix)
import Task
import Date exposing (Date)

-- 日付を比較する関数
compareDates : Date -> Date -> Order
compareDates date1 date2 =
    Date.compare date1 date2

-- 実用例を見てみよう
example : Task.Task x String
example =
    Task.map (\o ->
        case o of
            LT ->
                "第一の日付の方が古いよ"

            EQ ->
                "二つの日付は同じだね"

            GT ->
                "第一の日付の方が新しいね"
    )
    (Task.map2 compareDates (Date.fromPosix (Posix 0)) (Date.fromPosix (Posix 86400000)))
```

サンプル出力の ```
"第一の日付の方が新しいね"
``` というのは、第一の日付が第二の日付より一日新しい場合の結果だね。

## Deep Dive
Elmの日付比較は、JavaScriptより簡潔でエラーが少ない。`Date` モジュールはElmの初期バージョンからあるが、今ではより安全で使いやすくなっている。他の方法としては、日付をUnixタイムスタンプ（`Posix`）に変換して比較することもできる。実装の詳細については、Elmの内部で`compare`関数がどう動いてるかを理解することが大事だ。それぞれの日付を内部的な数値として見て、その数値を比較するんだ。

## See Also
- [Elm Date Documentation](https://package.elm-lang.org/packages/justinmimbs/date/latest/): `Date` モジュールの公式ドキュメンテーション
- [Elm Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/): `Time` モジュールに関する詳細
- [Elm Discuss](https://discourse.elm-lang.org/): Elmコミュニティでの議論ができるフォーラム
