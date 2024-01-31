---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:14:20.025658-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？

Elmで現在の日付を取得するのは日時データにアクセスする基本的な操作です。アプリでの記録、イベント、またはユーザーの時間帯に応じた機能を実装する際に行います。

## How to:
実装方法

```Elm
import Time exposing (Posix)
import Task

-- 関数が現在のPosix時間を取得する
getCurrentDate : Task.Task Time.Error Posix
getCurrentDate =
    Time.now

-- サンプル出力の例
-- Posix 1615195588123
```

## Deep Dive
深堀り

Elmでの現在の日付取得は`Time`モジュールによって実装されます。この操作はPOSIXタイムスタンプ（1970年1月1日からのミリ秒数）として表される`Posix`値を提供します。Elm 0.19以前では`Date`モジュールを使っていましたが、タイムゾーンの扱いが不十分だったため、`Time`モジュールに置き換えられました。代替方法としてサーバーから日付を取得することも可能ですが、それは追加の依存性と遅延を引き起こすかもしれません。Elmでは`Time.now`を使った取得が一般的で、単純で信頼性の高い方法です。

## See Also
関連する情報源

- Elmの公式`Time`モジュールドキュメント：[Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- POSIX時間についての詳細：[Wikipedia POSIX Time](https://en.wikipedia.org/wiki/Unix_time)
- タイムゾーンの扱いについての参考情報：[Elm Time.Zone](https://package.elm-lang.org/packages/elm/time/latest/Time-Zone)
