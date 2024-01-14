---
title:    "Elm: 「未来または過去の日付の計算」"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ
今日、私たちはプログラミング言語の一つであるElmで日付の計算をすることについて説明します。私たちが日付を計算する必要がある理由は、私たちの日々の生活で日付を処理する必要があるからです。例えば、将来のイベントを計画する、過去のイベントを振り返るなど、様々な理由があります。Elmを使えば、日付の計算が簡単に行えるので、ぜひご紹介します！

# 使い方
まずは簡単な例を見てみましょう。今日の日付から1週間後の日付を計算するには、次のようにElmコードを書くことができます。

```Elm
import DateTime exposing (add, days)
import Time exposing (2weeks)

weekAfterToday = add days 7 2weeks
```

上のコードでは、`DateTime`パッケージを使って今日の日付から1週間後の日付を計算し、`Time`パッケージを使って結果を2週間後の日付として出力しています。

さらに、特定の日付から特定の日数を足したり引いたりすることも可能です。例えば、私たちが何日後の日付を計算したい場合、`add`関数を使います。その際、足したい日数を引数として渡します。

```Elm
import DateTime exposing (add, days, fromString)
import Time

-- 今日の日付を定義
today = fromString "2019-01-01"

-- 2週間後の日付を計算
twoWeeksLater = add days 14 today
```

上のコードでは、`DateTime`パッケージの`fromString`関数を使って今日の日付を定義し、`add`関数を使って2週間後の日付を計算しています。

# 深堀り
また、特定の日付から別の日付までの日数を計算することもできます。この場合、`DateTime`パッケージの`diffInDays`関数を使います。

```Elm
import DateTime exposing (diffInDays, fromCalendarDate, toCalendarDate)

-- 日数を計算したい日付を定義
fromDate = fromCalendarDate 2019 1 1
toDate = fromCalendarDate 2019 2 1

-- 日数を計算
daysBetween = diffInDays fromDate toDate
```

上のコードでは、`DateTime`パッケージの`fromCalendarDate`関数を使って2つの日付を定義し、`diffInDays`関数を使ってその日付の差を計算しています。結果は`31`になります。

以上がElmで日付を計算する方法です。このように、Elmは日付の計算を簡単に行える優れた機能があります。ぜひ使ってみてください！

# 他の記事も見てみよう
- Elm公式ドキュメント（日付操作）：https://guide.elm-lang.jp/datetime/
- Elm公式パッケージリポジトリ：https://package.elm-lang.org/packages/elm/core/latest/DateTime