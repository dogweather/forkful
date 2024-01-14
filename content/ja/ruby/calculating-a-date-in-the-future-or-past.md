---
title:    "Ruby: 将来または過去の日付を計算する"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

Rubyプログラミングの日本語ブログポスト

## Why

なぜ日付を過去や将来に計算する必要があるのでしょうか？日付を計算することには、たくさんの理由があります。たとえば、プロジェクトの締め切りを管理するために、将来の日付を計算することができます。また、過去の日付を計算することで、歴史的なイベントや記念日を追跡することもできます。

## How To

日付の計算はRubyでとても簡単に行うことができます。まず、`Date`ライブラリを`require`しましょう。それから、次のようにコードを書くことで、現在の日付を取得することができます。

```Ruby
require 'date'

today = Date.today
```

このコードを実行してみると、今日の日付が表示されます。

次に、将来の日付を計算する方法を見てみましょう。例えば、1ヶ月後の日付を計算するには、次のようにコードを書きます。

```Ruby
require 'date'

next_month = Date.today + 1 month
```

今の日付に1ヶ月を足すことで、来月の日付が計算されます。同様に、過去の日付を計算するには、`-`演算子を使います。例えば、3ヶ月前の日付を計算するには、次のようにコードを書きます。

```Ruby
require 'date'

three_months_ago = Date.today - 3 months
```

これで、3ヶ月前の日付が計算されます。

## Deep Dive

日付を計算する際に気を付けるべきポイントは、閏年やタイムゾーンの影響を受けることです。また、特定の曜日の日付を計算したい場合には、`wday`メソッドを使うことができます。さらに、時刻も一緒に計算したい場合には`DateTime`ライブラリを使うことで実現できます。

## See Also

- [Ruby ドキュメンテーション - Dateクラス](https://docs.ruby-lang.org/ja/2.6.0/class/Date.html)
- [Ruby ドキュメンテーション - DateTimeクラス](https://docs.ruby-lang.org/ja/2.6.0/class/DateTime.html)
- [Rubyで日付の計算 - Qiita](https://qiita.com/ryouzi/items/7213f6edd1a776100310)
- [Rubyにおける日付計算の基本 - wonderworker](https://wonderworker.jp/2015/02/05/9466/)