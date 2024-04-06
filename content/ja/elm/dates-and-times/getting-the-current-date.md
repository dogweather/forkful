---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:30.560129-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elm\u306F`Time`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3067\u65E5\u4ED8\u3092\u6271\u3044\u307E\u3059\u3002\u73FE\
  \u5728\u306E\u6642\u523B\u3092POSIX\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3068\
  \u3057\u3066\u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\u65E5\u4ED8\u306B\u5909\u63DB\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.566221-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elm\u306F`Time`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u3067\u65E5\u4ED8\u3092\u6271\u3044\u307E\u3059\u3002\u73FE\u5728\
  \u306E\u6642\u523B\u3092POSIX\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3068\u3057\
  \u3066\u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\u65E5\u4ED8\u306B\u5909\u63DB\u3057\
  \u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## どのようにして：
Elmは`Time`モジュールで日付を扱います。現在の時刻をPOSIXタイムスタンプとして取得し、それを日付に変換します。

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- POSIX時刻を日付レコードに変換
                date = Time.toDate posixTime
            in
            -- ここでモデルを適切に更新
            ({ model | date = date }, Cmd.none)

-- 現在の時刻を取得するための初期化
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- 例出力：
-- date { year = 2023, month = Mar, day = 26 }
```

## 詳細
古いWeb言語では、日付を取得するのは1行のコードです。Elmは異なります。Elmアーキテクチャを通じて、現在の時刻のような副作用を明示的にすることです。これにより、コードの純粋性と保守性が促進されます。

代替案には、サードパーティのパッケージを使用するか、日付をサーバーコードで扱い、フラグやポートを通じてElmに渡す方法があります。

実装としては、Elmの`Time.now`はPOSIXタイムスタンプ（Unixエポック以来のミリ秒）として時刻を取得します。これはタイムゾーンに依存せず、必要に応じて`Time`モジュールの関数を使用してフォーマットできます。

## 参照
- [Elm Time ドキュメント](https://package.elm-lang.org/packages/elm/time/latest/)
- [コマンドとサブスクリプションに関するElmのガイド](https://guide.elm-lang.org/effects/)
