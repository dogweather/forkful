---
title:    "C: 現在の日付を取得する"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
プログラマーにとって、現在の日付を取得することは重要なタスクの一つです。例えば、ファイル名やログファイルのタイムスタンプを自動的に付ける必要がある場合や、特定の日付に処理を制限する必要がある場合など、様々なシナリオで日付を取得することが必要になります。

## 方法
C言語では、現在の日付を取得するためにtime.hヘッダーファイルを使用します。まずはこのヘッダーファイルをインクルードしましょう。

```C
#include <time.h>
```

次に、struct tmという構造体を使用して現在の日付を取得します。struct tmは時刻、月、日、年などの情報を保持するための構造体です。

```C
struct tm current_date = *localtime(time(NULL));
```

この方法では、現在の日付を取得し、current_date変数に格納しています。ただし、このままでは読みやすい形ではないため、strftime()関数を使用して現在の日付を任意の形式で出力することができます。

```C
strftime(date_str, 50, "%Y-%m-%d", &current_date); // date_strは現在の日付の文字列を格納するバッファ
printf("現在の日付は%sです。", date_str); // 現在の日付は2021-10-20です。
```

## ディープダイブ
現在の日付を取得するには、time()関数を使用します。time()関数はUTC時間からの経過秒数を返すため、そのままでは読みやすい形ではありません。そのため、localtime()関数を使用して、現在のローカルタイムを返すように修正します。

また、time_t型の変数に経過秒数を格納することで、より詳細な情報を取得することもできます。例えば、tm_sec、tm_min、tm_hourなど、それぞれ時間、分、秒の情報を取得することができます。

```C
time_t current_time = time(NULL);
struct tm *current_date = localtime(&current_time);
int hour = current_date->tm_hour; // 現在の時を取得
int minute = current_date->tm_min; // 現在の分を取得
int second = current_date->tm_sec; // 現在の秒を取得
```

## 同様に

[time.hの公式ドキュメント](https://www.ibm.com/docs/en/zos/2.2.0?topic=functions-time-time-date-and-time-type)、 [strftime()関数の使用方法](https://www.man7.org/linux/man-pages/man3/strftime.3.html)、 [ローカルタイムの取得方法](https://stackoverflow.com/questions/1442116/how-to-get-date-and-time-value-in-c-program/)を参考にしてください。

## 同様に
[time.hヘッダーファイルのチュートリアル](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)、 [構造体の基本](https://www.tutorialspoint.com/cprogramming/c_structures.htm)、 [Markdownの基本](https://www.markdownguide.org/basic-syntax/)も参考にしてください。