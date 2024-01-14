---
title:    "Elixir: 「現在の日付を取得する」"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
現在の日付を取得することに取り組む理由を説明します。

日付を取得することは、プログラムを書く際に頻繁に必要となるタスクです。例えば、アプリケーションで現在の日付を表示したり、ロギングのために日付を記録したりする必要があります。Elixirでは、日付を取得するための組み込み関数が用意されており、簡単に実装することができます。

## 方法
```Elixir
Date.utc_today()
```

上記のコードを実行すると、UTC時間で現在の日付が取得できます。また、日付を特定のフォーマットで表示するには、`strftime`関数を使用します。

```Elixir
Date.utc_today() |> Date.strftime("%Y-%m-%d")
```

このようにすると、例えば2020年9月15日のように、指定したフォーマットで日付を取得することができます。

## 深堀り
Elixirでは、日付を扱うためのさまざまなライブラリが存在します。例えば、`timex`ライブラリを使うと、より複雑な日付の操作やアプリケーションでの日付の処理を行うことができます。

また、日付だけでなく、時刻やタイムゾーン、曜日などを取得することもできます。詳細な情報については、Elixirの公式ドキュメントやコミュニティの情報を参考にすることをお勧めします。

## 参考リンク
[日付と時刻 - Elixir 公式ドキュメント](https://hexdocs.pm/elixir/Date.html)

[timex ライブラリ - GitHubページ](https://github.com/bitwalker/timex)

[Elixir 日本ユーザーグループ](https://elixir-jp.github.io/)