---
title:                "Ruby: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

何か特定の日付を計算することが必要な場合、Rubyを使えば簡単に実現できます。例えば、未来の日付を知りたい場合や過去の日付を調べたい場合などです。

## 使い方

Rubyでは`Date`クラスを使って簡単に日付の計算ができます。まず、必要なライブラリをインポートしましょう。

```Ruby
require "date"
```

今日の日付を取得するには、`Date.today`を使います。

```Ruby
today = Date.today
puts today
```

出力結果はこのようになります。

```Ruby
2021-04-13
```

未来の日付を計算するには、`+`演算子を使います。例えば、1日後の日付を知りたい場合は以下のようにコードを書きます。

```Ruby
future_date = today + 1
puts future_date
```

出力結果はこちらです。

```Ruby
2021-04-14
```

過去の日付を計算するには、`-`演算子を使います。例えば、1年前の日付を知りたい場合は以下のようにコードを書きます。

```Ruby
past_date = today - 365
puts past_date
```

出力結果はこちらです。

```Ruby
2020-04-13
```

## 詳細な情報

`Date`クラスでは、年・月・日などの日付情報を取得することもできます。詳しくは公式ドキュメントを参照してください。

その他にも、Rubyでは日付や時刻を扱うための多くの便利なメソッドが用意されています。詳しい使い方や注意点は公式ドキュメントやコミュニティフォーラムで確認することができます。

## 関連リンク

- [Ruby公式ドキュメント (日付と時刻)](https://docs.ruby-lang.org/ja/latest/class/Date.html)
- [Rubytips: 日付と時刻を扱う方法](https://rubytips.net/8)
- [teratail: Rubyにおける日付・時刻の扱い方](https://teratail.com/questions/111692)