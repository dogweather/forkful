---
title:                "2つの日付の比較"
html_title:           "Bash: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日々のプログラミング作業で日付を比較することはよくあることです。例えば、ある特定の日付よりも前に作成されたファイルを見つける場合や、ある日付よりも後の日付を含むデータを検索する場合などです。Bashを使ってどのように簡単にこれらの日付を比較することができるかを学びましょう。

## 方法

まず、2つの日付を以下のようなスタイルで設定します。

```Bash
date1="2021/01/01" #比較する1つ目の日付
date2="2021/02/27" #比較する2つ目の日付
```

日付を比較するには、以下のようにif文を使用します。

```Bash
if [ "$date1" -lt "$date2" ]; then
    echo "日付1の方が日付2よりも前です"
elif [ "$date1" -gt "$date2" ]; then
    echo "日付2の方が日付1よりも前です"
else
    echo "2つの日付は同じです"
fi
```

上記のように、Bashでは「-lt」オプションを使用して日付を比較することができます。その他のオプションとしては、「-le」（以下）、 「-gt」（より大きい）、 「-ge」（以上）、 「-eq」（等しい）、 「-ne」（等しくない）があります。

また、日付を日付として認識させるためには「date -d」を使用することができます。例えば、曜日を確認する場合、以下のように書くことができます。

```Bash
weekday="Date" #調べたい曜日を入力
date -d "$weekday" +%u
```

この場合、日曜日であれば「7」、土曜日であれば「6」、金曜日であれば「5」といったように、数字が返されます。

## ディープダイブ

上記で紹介した方法以外にも、日付を比較するための様々な方法があります。例えば、「DATEコマンド」や「文脈依存の形式」の使用などです。また、UNIX時間を使用することで日付を比較することもできます。さらに、スクリプトを作成したり、変数を使用したりすることで、より複雑な日付の比較が可能となります。

## 関連リンク

- [Bashドキュメント](https://www.gnu.org/software/bash/)
- [UNIX時間に関する詳しい記事](https://www.unixtimestamp.com/)
- [Bashでの日付比較の方法についての解説](https://linuxhint.com/bash_date_comparison/)
- [DATEコマンドについての詳細](https://www.computerhope.com/unix/bash/date.htm)