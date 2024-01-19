---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

日付を取得する手法は、現在時刻を取得し、その情報を範囲内のコードで使用するためのものです。これは、ログ出力、行動のタイムスタンプ付け、スケジュールの計算など、時間依存の機能を持つプログラムに不可欠です。

## 実装方法:

Elixirで現在の日付を取得するのは簡単です。以下にそのコードを示します。

```Elixir
DateTime.utc_now() |> DateTime.to_date()
```
これは以下のような出力を生成します:

```Elixir
~D[2022-02-02]
```
このコードは、DateTime モジュールの `utc_now/0` 関数を使用して現在の日時を取得し、`to_date/1` 関数を使用して日付部分を抽出します。

## さらに深く:

Elixir での日時取得の以下の点について深く探っていきましょう。

1. **歴史的な文脈**: ElixirのDateTimeモジュールは、ISO8601日付と時間の標準をサポートしています。これにより、日付と時間の操作が容易になり、他のシステムとの互換性も確保されます。

2. **代替手段**: `DateTime.now("Asia/Tokyo")` というコードも存在します。これは現地時間を取得する方法です。ただし、一般的にはUTC時間を使用し、必要に応じてローカル時間に変換することが推奨されています。

3. **実装詳細**: Elixir の日付・時間の関数は、Erlang の calendar モジュールによって提供された低級の関数を使用しています。これにより、Elixirの時間関数は高効率で信頼性があります。

## 参照情報:

以下のリンクで、さらに情報を得ることができます。

- Elixir公式ドキュメントのDateTimeモジュール: https://hexdocs.pm/elixir/DateTime.html
- Elixir公式ドキュメントの Date モジュール: https://hexdocs.pm/elixir/Date.html
- ISO8601の詳細: https://www.iso.org/iso-8601-date-and-time-format.html