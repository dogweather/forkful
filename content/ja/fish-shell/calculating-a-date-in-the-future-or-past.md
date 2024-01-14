---
title:                "Fish Shell: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
どのような日付を過去や未来に計算する必要があるのか、簡単に説明します。

## 方法
```Fish Shell```を使用して、日付を計算するためのコーディング例とサンプル出力を以下に示します。

### 過去の日付を計算する
たとえば、10日前の日付を計算したい場合、以下のように入力します。

```
set today (date -u +"%m%d")
set ten_days_ago (math $today - 10)
set date (date -ud "$ten_days_ago" +"%Y-%m-%d")
echo $date
```

出力は、現在の日付から10日前の日付がYYYY-MM-DDの形式で表示されます。

### 未来の日付を計算する
同様に、10日後の日付を計算するには、以下のコードを使用します。

```
set today (date -u +"%m%d")
set ten_days_later (math $today + 10)
set date (date -ud "$ten_days_later" +"%Y-%m-%d")
echo $date
```

出力は、同じく現在の日付から10日後の日付がYYYY-MM-DDの形式で表示されます。

## 詳細
日付を計算する際に使われる```date```コマンドは様々なオプションがあり、特定の日付や時刻のフォーマットや、タイムゾーンの設定も可能です。また、```math```コマンドを使用することで、日付の計算を簡単に行うことができます。

## 参考リンク
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [日付と時刻を操作する方法](https://qiita.com/nishina555/items/7ea9587c2c2a42a9aa1f)