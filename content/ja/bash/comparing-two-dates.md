---
title:                "Bash: 二つの日付を比較する"
simple_title:         "二つの日付を比較する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することの重要性を例を挙げて簡単に説明する。

日付の比較は、日常生活でもよく行われることです。例えば、誕生日や結婚記念日などのイベントの日付を確認する、特定の期間内のデータを分析するために日付を比較する、など。日付の比較をコンピュータープログラミングで行うことで、より効率的に作業を進めることができます。

## 方法

日付を比較するには、Bashプログラミング言語を使用します。以下のコードブロックの例を参考にしてください。

```Bash
# 日付をランダムに生成
date1=$(( (RANDOM % 31) + 1 ))
month1=$(( (RANDOM % 12) + 1 ))
year1=$(( (RANDOM % 10) + 2020 ))

date2=$(( (RANDOM % 31) + 1 ))
month2=$(( (RANDOM % 12) + 1 ))
year2=$(( (RANDOM % 10) + 2020 ))

# 日付を比較して出力
if [ $year1 -lt $year2 ]; then
  echo "$year1年$month1月$date1日は$year2年$month2月$date2日より前です。"
elif [ $year1 -gt $year2 ]; then
  echo "$year1年$month1月$date1日は$year2年$month2月$date2日より後ろです。"
else 
  if [ $month1 -lt $month2 ]; then
    echo "$year1年$month1月$date1日は$year2年$month2月$date2日より前です。"
  elif [ $month1 -gt $month2 ]; then
    echo "$year1年$month1月$date1日は$year2年$month2月$date2日より後ろです。"
  else 
    if [ $date1 -lt $date2 ]; then
      echo "$year1年$month1月$date1日は$year2年$month2月$date2日より前です。"
    elif [ $date1 -gt $date2 ]; then
      echo "$year1年$month1月$date1日は$year2年$month2月$date2日より後ろです。"
    else 
      echo "$year1年$month1月$date1日は$year2年$month2月$date2日と同じです。"
    fi
  fi
fi
```

上記のコードを実行すると、２つの日付の関係に応じてメッセージが出力されます。

## 深堀り

２つの日付を比較する際には、年、月、日の順番で比較する必要があります。また、日付を比較する際には時差や閏年なども考慮する必要があります。

Bashプログラミング言語では、`date`コマンドを使用することで、現在の日付や指定した日付を取得することができます。また、`if`文を使用することで、条件に応じて処理を分岐させることができます。

日付を比較する際の注意点として、`-lt`や`-gt`といった比較演算子を使う際には、比較する数値を空白なしで記述する必要があります。

## 参考リンク

- [Bashのif文の使い方](https://www.atmarkit.co.jp/ait/articles/2003/06/news016_2.html)
- [Bashコマンドの使い方](https://develovment-based.com/greet/basics-of-bash-command/)
- [Bashでの日付