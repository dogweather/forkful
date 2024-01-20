---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？
現在の日付を取得するということは、プログラマが現在の日時情報をアプリケーションで利用するための手段です。この情報はログの生成、スケジュール管理、限定的なイベントのトリガーによく使われます。

## 方法：
以下、Elmプログラムで現在の日付を取得するサンプルコードとその出力を示します：

```Elm
import Task
import Time
import Browser

main =
  Time.now
      |> Task.perform (Debug.log "現在のミリ秒: ")
      
-- 出力: ("現在のミリ秒: ",1609459200000)

```
このサンプルでは、Elmの `Time.now` を使用して現在の時間をミリ秒単位で取得します。

## 詳細:
1. 歴史的な文脈: Elmは、純粋な関数型プログラミング言語として設計されました。現在の日付や時刻を取得する機能は、副作用を引き起こする可能性があるため、Elmでは `Task` を使用してこれを一般的に管理します。
   
2. 代替手段: Elmでは、基本的には `Time` モジュールの `now` 関数を利用しますが、特定の時刻フォーマットが必要な場合は、 `Time.Posix` を利用することも一般的です。

3. 実装詳細: Elmの `Time.now` 関数は、現在のユニックス時間（1970年1月1日からの経過ミリ秒）を返します。

## 参考情報:
- Elmの公式ドキュメンテーションの [Time](https://package.elm-lang.org/packages/elm/time/latest/) モジュール
- Elmの公式ドキュメンテーションの [Task](https://package.elm-lang.org/packages/elm/core/latest/Task) モジュール