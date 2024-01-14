---
title:    "C: 未来または過去の日付を計算する"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ

日付を計算する必要がある理由はさまざまです。例えば、誕生日や記念日に特定の日付を計算するのに役立ちます。また、将来のイベントの日付を把握したい場合や、過去の出来事の日付を確認する必要がある場合にも役に立ちます。C言語を使用すると、日付を計算することは非常に簡単で効率的です。

## 使い方

C言語を使用して日付の計算を行う方法はいくつかありますが、ここでは ```time.h``` ライブラリを使用した方法を紹介します。まず、必要なライブラリをインクルードします。

```
#include <stdio.h>
#include <time.h>
```

次に、時間構造体を定義し、日付を入力します。

```
struct tm input_date = {0};

//入力したい日付を"年,月,日" の順番で入力します
input_date.tm_year = 年 - 1900; 
input_date.tm_mon = 月 - 1;
input_date.tm_mday = 日;
```

日付を計算したい場合は、 ```timeval``` 構造体を使用します。例えば、1週間後の日付を計算するには次のようにします。

```
struct timeval calculated_date;
calculated_date.tv_sec = mktime(&input_date) + (7*24*60*60);
```

計算した日付を読みやすい形式で表示するために、```strftime``` 関数を使用します。

```
char date_str[100];
strftime(date_str, 100, "%Y年%m月%d日", localtime(&calculated_date.tv_sec));
printf("1週間後の日付は%sです。", date_str);
```

上記のコードを実行すると、以下のように表示されます。

```
1週間後の日付はXXXX年XX月XX日です。
```

## 詳細を深堀りする

日付の計算にはさまざまなアプローチがあります。日付の加算や減算だけでなく、曜日の計算や特定の日付が含まれる月の数の計算なども行うことができます。また、柔軟性のある計算を行うためには、日付の表現方法を標準タイムゾーンで行う必要があります。

さらに、C言語の日付計算には欠点もあります。例えば、サマータイムやうるう年の扱いなど、特別なケースを考慮する必要があります。そのため、より複雑な日付の計算を行う場合は、別の言語やライブラリを使用することも検討する必要があります。

## 参考リンク

- [C言語で日付を計算する方法](https://www.geeksforgeeks.org/date-after-adding-given-number-of-days-to-the-given-date/)
- [strftime関数の使い方](https://man7.org/linux/man-pages/man3/strftime.3.html)
- [時間や日付の計算に役立つC言語ライブラリ](https://linux.die.net/man/3/localtime)