---
date: 2024-01-20 17:30:51.659450-07:00
description: "How to: (\u3084\u308A\u65B9) Elixir \u3067\u5C06\u6765\u307E\u305F\u306F\
  \u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u57FA\u672C\u306F `DateTime`\
  \ \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\
  \u3053\u3053\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u6319\u3052\u307E\u3059."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.574878-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Elixir \u3067\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\
  \u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u57FA\u672C\u306F `DateTime` \u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u3053\u3053\
  \u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u6319\u3052\u307E\u3059."
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
