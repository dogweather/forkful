---
title:    "C: 現在の日付の取得"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ日付を取得するのか

多くのプログラムにとって、現在の日付を取得することは非常に重要です。例えば、あなたが作成したプログラムを使用したユーザーにとって、現在の日付が何であるかを知ることは便利です。また、今日の日付を使用してファイルやフォルダを作成することもできます。さらには、あるプロジェクトが何日間続いているのかを知るためにも、現在の日付を取得する必要があります。

## 方法

C言語を使用して、現在の日付を取得する方法を説明します。まず、<time.h>ヘッダーファイルをインポートします。そして、現在の日付を格納するための変数を宣言します。最後に、time関数を使用して、現在の日付を取得し、変数に格納します。

```
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now;
    time(&now);

    printf("今日の日付は: %s", ctime(&now));

    return 0;
}
```

このコードを実行すると、以下のような出力が得られます。

```
今日の日付は: Mon Apr 19 20:03:32 2021
```

## ディープダイブ

現在の日付を取得するために使用されているtime関数についてもう少し詳しく説明します。time関数はUnixエポックからの経過秒数を返すため、取得した経過秒数からタイムスタンプを作成することもできます。

また、ctime関数を使用することで、取得した経過秒数から読みやすい形式の日付を取得することもできます。

## 参考リンク

- [C言語 - 現在の日付を取得する方法](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
- [C言語の<time.h>ヘッダーファイル](https://www.atmarkit.co.jp/ait/articles/1705/12/news024.html)
- [ctime関数の使い方](https://www.ibm.com/docs/jp/aix/731/systemsmanagement/c-datetime-library-functions-mman/ctime.html)