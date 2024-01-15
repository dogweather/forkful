---
title:                "日付を文字列に変換する"
html_title:           "Arduino: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの利点は、日付をよりわかりやすく、扱いやすい形式にすることです。例えば、データベースやログファイルに日付の情報を保存する際や、表示する際に便利です。

## 使い方
日付を文字列に変換するには、まず `time.h` ライブラリをインクルードします。次に、`tm` 構造体を定義し、変換したい日付の情報を入力します。最後に、`strftime()` 関数を使って日付を文字列に変換します。

```Arduino
#include <time.h>

// 年月日のデータを定義
int year = 2020;
int month = 12;
int day = 31;

// tm構造体に日付の情報を入力
tm date = {
  .tm_year = year - 1900,
  .tm_mon = month - 1,
  .tm_mday = day
};

// 2020-12-31の形式の文字列に変換し、Serial Monitorに出力
char str[11];
strftime(str, 11, "%Y-%m-%d", &date);
Serial.println(str);
```

上記のコードを実行すると、`2020-12-31` という文字列が出力されます。

## 詳しく調べる
`strftime()` 関数は、日付を任意の書式に変換することができます。第二引数に書式を指定することで、様々な表示形式の文字列を作成することができます。例えば、`%Y-%m-%d` の他にも、`%m/%d/%Y` という書式を指定することで、`12/31/2020` という文字列を作成することができます。

また、`time.h` ライブラリには、日付を取得したり操作したりするための便利な関数がたくさんあります。ぜひ調べて、さまざまな日付処理を実装してみてください。

## 関連情報を見る
- [strftime() 関数のドキュメント](https://www.arduino.cc/reference/jp/language/functions/time/strftime/)
- [`time.h` ライブラリのドキュメント](https://www.arduino.cc/reference/jp/language/functions/time/)