---
date: 2024-01-20 17:30:51.659450-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u7279\u5B9A\u306E\u6642\
  \u9593\u3092\u8DB3\u3057\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\u3066\u65B0\u3057\
  \u3044\u65E5\u4ED8\u3092\u5F97\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u671F\u9650\u306E\u7BA1\u7406\u3001\u4E88\
  \u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u307E\u305F\u306F\u5C65\u6B74\u30C7\u30FC\u30BF\
  \u306E\u5206\u6790\u3067\u3088\u304F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.783517-07:00'
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u7279\u5B9A\u306E\u6642\
  \u9593\u3092\u8DB3\u3057\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\u3066\u65B0\u3057\
  \u3044\u65E5\u4ED8\u3092\u5F97\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u671F\u9650\u306E\u7BA1\u7406\u3001\u4E88\
  \u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u307E\u305F\u306F\u5C65\u6B74\u30C7\u30FC\u30BF\
  \u306E\u5206\u6790\u3067\u3088\u304F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
将来または過去の日付を計算するっていうのは、特定の時間を足したり引いたりして新しい日付を得るプロセスです。プログラマーは、期限の管理、予約システム、または履歴データの分析でよくこれを行います。

## How to: (やり方)
Elixir で将来または過去の日付を計算する基本は `DateTime` モジュールを使うことです。ここに基本的な例を挙げます:

```elixir
# 現在の日時を取得
current_time = DateTime.utc_now()

# 10日後の日時を計算
ten_days_later = DateTime.add(current_time, 10 * 24 * 60 * 60)

# 出力
IO.puts(DateTime.to_string(ten_days_later))
```

たったこれだけで、10日後の日時が表示されます。

```elixir
# 現在の日時
current_time = DateTime.utc_now()

# 30日前の日時を計算
thirty_days_ago = DateTime.add(current_time, -30 * 24 * 60 * 60)

# 出力
IO.puts(DateTime.to_string(thirty_days_ago))
```

これで30日前の日時を簡単に得ることができます。

## Deep Dive (深く掘り下げて)
Elixir の `DateTime` クラスは、時刻計算を扱うのにとても便利です。Elixir には以前は `:calendar` モジュールがありましたが、より洗練された機能を提供する `DateTime` に置き換わりました。日時計算では、時間単位の変換（秒、分、時間）を意識する必要があることに注意してください。この例では、日数を秒数に変換しています。`DateTime.add/3` 関数は、`DateTime` 構造体と追加したい秒数、そして時間帯情報をオプションとして受け取ります。

タイムゾーンを扱う場合は `Timezone` モジュールが役に立ちますが、上の例では `DateTime.utc_now/0` を用いて協定世界時（UTC）をベースにしています。他のライブラリや方法もありますが、Elixir 標準のモジュールを使ってもかなり柔軟に多くのことができます。

## See Also (関連情報)
- Elixir の公式ドキュメント: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- タイムゾーンの扱いについて: [https://hexdocs.pm/tzdata/Tzdata.html](https://hexdocs.pm/tzdata/Tzdata.html)
