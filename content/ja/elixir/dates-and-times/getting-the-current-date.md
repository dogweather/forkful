---
title:                "現在の日付の取得"
date:                  2024-02-03T19:09:47.660476-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
  - 2024-02-05, dogweather, reviewed and corrected
lastmod:              2024-02-05
---

{{< edit_this_page >}}

## 何となぜ？
Elixirで現在の日付を取得するには、システムの日付と時刻の情報にアクセスすることが関わってきます。これは、ログ記録、データスタンピング、または現在の日付の知識が必要なあらゆる機能のためによく行われるタスクです。この操作は、時間を意識したアプリケーションを作成するために不可欠であり、ウェブアプリケーションでのレポート生成やタイムスタンプの作成といったタスクにおいても重要です。

## 方法：
Elixirの標準ライブラリは、`DateTime`モジュールを通じて、現在の日付と時刻を取得することを可能にします。ElixirはErlang VM（BEAM）上で動作するため、時刻操作のための基底となるErlangの機能を利用します。

### Elixirの標準ライブラリを使用する
Elixirは`DateTime.utc_now/0`関数を提供して、UTCでの現在の日付と時刻を取得します。

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**サンプル出力:**
```
~U[2024-02-05 19:58:40.925931Z]
```

現在の日付だけを取得したい場合は、年、月、日のコンポーネントを抽出することができます：

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**サンプル出力:**
```
~D[2023-05-04]
```

### Timexライブラリを使用する
より複雑な日付と時刻の要求に対しては、Timexという人気のあるサードパーティライブラリを利用できます。まず、mix.exsの依存関係に`Timex`を追加します：

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

依存関係をインストールした後（`mix deps.get`）、Timexを使って現在の日付を取得できます：

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**サンプル出力:**
```
~D[2023-05-04]
```

Timexは日付と時刻の操作に関する広範な機能を提供しており、特にタイムゾーン、フォーマット、日付と時刻のパースを扱う際に、あなたのElixirアプリケーションに強力な追加機能として働きます。

Elixirの組み込み機能とTimexライブラリの両方を理解し、利用することで、Elixirアプリケーションで日付と時刻を簡単に扱えるようになり、アプリケーションのニーズに合わせて精度と使いやすさで体験を調整できます。
