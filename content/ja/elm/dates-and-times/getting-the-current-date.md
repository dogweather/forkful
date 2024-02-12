---
title:                "現在の日付の取得"
aliases: - /ja/elm/getting-the-current-date.md
date:                  2024-02-03T19:09:30.560129-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Elmで現在の日付を取得するとは、システムから現在のカレンダー日付を取り出すことを意味します。イベントのタイムスタンプ、タスクのスケジュール、または期間の追跡を行うためにこれを行います。

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
