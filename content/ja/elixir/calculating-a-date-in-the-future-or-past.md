---
title:                "Elixir: 未来または過去の日付の計算方法"
simple_title:         "未来または過去の日付の計算方法"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

未来や過去の日付を計算することについて、なぜ誰かが参加するかをわかりやすく説明します。

## 方法

以下の```Elixir```コードブロック内に、コーディングの例と出力サンプルを記載しています。

```Elixir
# 今日の日付を取得
today = Date.utc_today()

# 1週間後の日付を計算
next_week = Date.add(today, 7)

# 日付のフォーマットを指定して出力
IO.puts "今日から1週間後の日付は #{Date.to_string(next_week, "{YYYY年MM月DD日}")} です。"
```

出力例:

```
今日から1週間後の日付は 2021年06月17日 です。
```

## 深堀り

未来や過去の日付を計算する方法はいくつかあります。例えば、```Date.add/2```関数を使用する方法や、```Date.new!/3```関数を使用し、日付の値を手動で指定する方法があります。また、日付を計算する際には、タイムゾーンやクリスマスや年末年始などの特殊な日付にも注意が必要です。

## もっと詳しくは

「深堀り」のセクションで説明した方法以外にも、未来や過去の日付を計算する方法は様々あります。興味がある方は以下のリンクを参考にしてみてください。  

### Elixirの公式ドキュメント
https://hexdocs.pm/elixir/Date.html

### ブログ記事「Elixirで日付を取得する方法」
https://blog.yuyat.jp/post/elixir-date/

### ブログ記事「Elixirで年末年始の日付を計算する方法」
https://qiita.com/morikat/items/53f0881c3efb1405c2c7

## 参考リンク

[See Also:](参考リンク)

- [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- [https://blog.yuyat.jp/post/elixir-date/](https://blog.yuyat.jp/post/elixir-date/)
- [https://qiita.com/morikat/items/53f0881c3efb1405c2c7](https://qiita.com/morikat/items/53f0881c3efb1405c2c7)