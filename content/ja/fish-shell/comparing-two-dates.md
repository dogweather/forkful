---
title:                "Fish Shell: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較する理由は何でしょうか？プログラミングにおいて、日付を比較することは非常に重要です。例えば、あるイベントが過去に起きたかどうかを判断するために日付を比較することがあります。また、後に起きるイベントよりも前の日付かどうかを確認する場合にも日付の比較が必要になります。この記事では、Fish Shellを使用して日付を比較する方法について紹介します。

## 方法

Fish Shellを使用すると、簡単に日付を比較することができます。まず、比較したい日付を「YYYY-MM-DD」の形式で記入します。次に、比較演算子「-lt」（より小さい）、「-gt」（より大きい）、または「-eq」（等しい）を使用します。例えば、「2019-12-01 -gt 2019-01-15」というコマンドを入力すると、左側の日付が右側の日付よりも大きいことを意味します。

```Fish Shell
set old_date 2019-12-01
set new_date 2019-01-15
```
```Fish Shell
if [ $old_date -gt $new_date ]
  echo "古い日付です"
end
```

この例では、「古い日付です」というメッセージが出力されます。また、日付の比較結果を変数に保存することもできます。例えば、以下のようにコマンドを入力することで、日付の比較結果を変数「result」に保存します。

```Fish Shell
set result $old_date -gt $new_date
```

## 深堀り

Fish Shellでは、日付だけでなく、日時を比較することも可能です。以下のように、「YYYY-MM-DDTHH:MM:SS」の形式で日時を指定することができます。また、特殊な比較演算子「-le」（より小さいまたは等しい）や「-ge」（より大きいまたは等しい）を使用することもできます。

```Fish Shell
set old_datetime 2019-12-01T08:30:00
set new_datetime 2019-05-30T12:45:00
```
```Fish Shell
if [ $old_datetime -gt $new_datetime ]
  echo "古い日時です"
end
```

また、日付や日時の加算や減算も可能です。例えば、以下のように「-1 day」や「+5 minutes」を使用すると、日付や日時を1日や5分だけ前後させることができます。

```Fish Shell
set new_date 2019-01-15
```
```Fish Shell
set added_date $new_date -1 day
set subtracted_date $new_date +5 minutes
```

## 関連リンク

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html#comparison)
- [日付の比較方法（Qiita）](https://qiita.com/tprl/items/411e161dbbea7a86f8c0)