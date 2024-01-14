---
title:                "Elm: 現在の日付の取得"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ現在の日付を取得する必要があるのか？

プログラミングにおいて、特定のタスクを実行する際には、時には現在の日付が必要になることがあります。例えば、予定やイベントを作成するとき、あるいはユーザーが最後にアクションを起こした日付を記録したいときなどがあります。そのような場合、現在の日付を取得することは非常に便利です。

## 方法

Elmで現在の日付を取得する方法はとても簡単です。```Date```モジュールを使用するだけで、現在の日付を取得できます。

```elm
import Date exposing (Date)
import Time exposing (Posix)

-- 現在の日付を取得
currentDate : Date
currentDate =
  Time.posixToMillis Posix.now |> Date.fromMillisSinceEpoch
```

以上のコードを実行すると、現在の日付が```currentDate```に格納されます。

また、特定の形式で日付を取得することも可能です。例えば、年、月、日のそれぞれの数値を取得したい場合は以下のようにコードを変更することができます。

```elm
import Date exposing (Date)
import Time exposing (Posix)

-- 現在の日付を取得
currentDate : ( Int, Int, Int )
currentDate =
  Time.posixToMillis Posix.now |> Date.fromMillisSinceEpoch |> Date.toGregorian
```

上記のコードを実行すると、現在の年、月、日がそれぞれの数値として取得されます。

## ディープダイブ

現在の日付を取得する際には、夏時間やタイムゾーンなど、さまざまな影響要因があります。そのため、取得した日付が正確であることを保証するためにはいくつかの追加処理が必要になります。詳しい情報は[公式ドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Time)を参照することをおすすめします。

## 関連情報

- [Elm公式ドキュメント](https://guide.elm-lang.org/)
- [楽しく学べる Elm 入門 - Qiita](https://qiita.com/ci7lus/items/9f70a6224e68ee3c7abc)
- [Elmってなんだ？ - Qiita](https://qiita.com/janus_wel/items/0c986741dcdd118f5f6b)