---
title:                "C: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラムを書くとき、時刻や日付を取得する必要があるかもしれません。例えば、あるアプリケーションを実行した日付や、データのタイムスタンプを確認するために必要になる場合があります。C言語では、現在の日付を取得するためにいくつかの方法があります。

## 方法

C言語で現在の日付を取得する方法は、非常にシンプルです。まず、`time.h`ヘッダーファイルをインクルードします。

```C
#include <time.h>
```

次に、`time()`関数を使用して現在の時刻を取得します。この関数は現在の時刻を秒単位の数値として返します。

```C
time_t now = time(NULL);
```

これで、現在の日付を取得することができました。ただし、このままでは時間を数値として扱うのは少々面倒です。そこで、`localtime()`関数を使用して日付や時刻の情報を`struct tm`構造体として取得することができます。

```C
struct tm *current = localtime(&now);
```

`struct tm`構造体には、`tm_year`（年）、`tm_mon`（月）、`tm_mday`（日）、`tm_hour`（時）、`tm_min`（分）、`tm_sec`（秒）などのメンバーが含まれています。これらのメンバーを使用して、日付や時刻を個別の変数に格納することができます。

```C
int year = current->tm_year + 1900;
int month = current->tm_mon + 1;
int day = current->tm_mday;
int hour = current->tm_hour;
int minute = current->tm_min;
int second = current->tm_sec;
```

これで、現在の日付と時刻を個別の変数として取得することができました。

## ディープダイブ

C言語では、`time_t`型を使用して時刻を表現します。`time_t`型は、プログラムを実行しているコンピューターの特定の時刻からの経過秒数を表します。つまり、エポック（Unixの起点となる日付）からの秒数を表しています。

しかし、`time_t`型には限界があり、2038年にはオーバーフローしてしまいます。そのため、2038年問題と呼ばれる大きな課題があります。この問題を回避するためには、複数の型を組み合わせて行うか、別の時刻の表現方法を利用する必要があります。

## 参考リンク

- [C言語で現在時刻を扱う方法](https://www.sejuku.net/blog/41543)
- [time.h - C/C++リファレンス](https://cpprefjp.github.io/reference/ctime/time.html)
- [2038年問題についての説明](https://ja.wikipedia.org/wiki/2038%E5%B9%B4%E5%95%8F%E9%A1%8C)

## 参考文献

- K&R、『プログラミング言語C 第2版』、日経BP社、1993年、1476ページ。
- Slavik, JP、『Unix プログラミング環境』上、Addison-Wesley、1987年、163ページ。