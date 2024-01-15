---
title:                "現在の日付を取得する"
html_title:           "C: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得するのに関わる理由はいくつかあります。例えば、プログラム内で実行された日付や日時に関する情報を記録する必要がある場合や、タイムスタンプを作成するために使用する場合などです。

## 方法

C言語を使用して現在の日付を取得する方法はいくつかあります。まずは、`time.h`ライブラリをインクルードする必要があります。

```
#include <time.h>
```

現在の日付を取得するには、`time()`関数を使用します。この関数は、システムの現在の時刻を取得し、秒単位の値を返します。

```
time_t now;
time(&now);
```

その後、`localtime()`関数を使用して、取得した秒単位の値を日付と時刻に変換します。

```
struct tm *local = localtime(&now);
```

最後に、`printf()`関数を使用して、取得した日付をフォーマットしたい形式で出力します。

```
printf("今日の日付は%02d/%02d/%dです", local->tm_mon + 1, local->tm_mday, local->tm_year + 1900);
```

上記のコードを実行すると、以下のような出力が得られます。

```
今日の日付は08/12/2021です
```

## ディープダイブ

`time()`関数や`localtime()`関数の詳細について深く知りたい方は、それぞれの関数のマニュアルページを参照してください。また、`time.h`ライブラリには、日付や時刻をより詳細に操作するための様々な関数が存在しますので、ぜひ調べてみてください。

## 参考リンク

- [time()関数のマニュアルページ](https://www.ibm.com/docs/ja/aix/7.1?topic=time-time-time-returns-the-current-calendar-time)
- [localtime()関数のマニュアルページ](https://www.ibm.com/docs/ja/aix/7.1?topic=time-localtime-localtime-converts)
- [time.hライブラリの参考ドキュメント](https://www.cs.utah.edu/~regehr/solid07/timelib/includeneeded/time.h.html)