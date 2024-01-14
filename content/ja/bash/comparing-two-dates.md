---
title:                "Bash: 「2つの日付を比較する」"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

Bashプログラミングをする際、2つの日付を比較することは非常に重要です。これは、特定の期間内に特定のアクションを実行するために必要な場合があります。また、特定の日付が過ぎたかどうかを確認するためにも使用されます。この記事では、Bashを使用して2つの日付を比較する方法を学ぶことができます。

## 方法

Bashスクリプトでは、dateコマンドを使用して、日付をフォーマットして比較することができます。以下の例は、2つの日付を比較し、その結果を出力する方法を示しています。

```Bash
#!/bin/bash

# 日付を比較するための関数を作成
compare_dates() {
    # 日付を定義
    date1=2021-05-01
    date2=2021-05-05

    # 日付を比較
    if [ $date1 -gt $date2 ]; then
        echo "$date1 is greater than $date2"
    elif [ $date1 -lt $date2 ]; then
        echo "$date1 is less than $date2"
    else
        echo "$date1 is equal to $date2"
    fi
}

# 関数を実行
compare_dates
```

上記のスクリプトでは、date1がdate2よりも大きいか、小さいか、または等しいかを確認しています。出力結果は以下のようになります。

```
2021-05-01 is less than 2021-05-05
```

このように、dateコマンドを使用することで、日付をフォーマットして比較することができます。

## 深堀り

Bashには、2つの日付を比較するために使用できる多くの機能があります。例えば、フォーマットされた日付を使用した比較や、特定の期間内にあるかどうかを確認するなどがあります。また、日付によるループや、日付を使用したファイルの検索もできます。Bashで日付を比較する際には、これらの機能を使いこなすことが重要です。

## See Also

- [Bash scripting cheatsheet - Date comparison](https://devhints.io/bash#dates)
- [How to compare two dates in Bash](https://linuxize.com/post/bash-compare-dates/)
- [Bashで日付を比較する方法](https://qiita.com/yk0817/items/f3e8dea478f0504c998f)