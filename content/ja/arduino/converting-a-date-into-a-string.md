---
title:                "Arduino: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの利点は多岐にわたります。日付を扱う際に、より柔軟に操作が可能になります。また、シリアルモニターなどで日付を読みやすい形式で表示することもできます。

## 方法

日付を文字列に変換するには、まず`String()`関数を使う必要があります。この関数は、データ型を文字列に変換する際に使用されます。例として、現在の日付を変数に保存し、`String()`関数を使って文字列に変換する方法を示します。

```Arduino
// 現在の日付を保存する
int day = 01;
int month = 07;
int year = 2021;

// 日付を文字列に変換する
String date = String(day) + '/' + String(month) + '/' + String(year);

// 日付をシリアルモニターに出力する
Serial.println(date);

```

上記のコードを実行すると、シリアルモニターには"01/07/2021"という日付が表示されます。

## ディープダイブ

日付を文字列に変換する際に、特に注意する必要があるのは日付のフォーマットです。上記の例では、ユーザーが指定した日付をそのまま文字列に変換していますが、一般的な日付フォーマットに沿った形式に変換する方法もあります。例えば、"2021年7月1日"のような形式で日付を表す場合は、`String()`関数ではなく`sprintf()`関数を使うと便利です。

```Arduino
int day = 01;
int month = 07;
int year = 2021;

// 日付を指定したフォーマットに変換する
char date[11];
sprintf(date, "%04d年%02d月%02d日", year, month, day);

// 日付をシリアルモニターに出力する
Serial.println(date);
```

上記のコードを実行すると、シリアルモニターには"2021年07月01日"という形式で日付が表示されます。フォーマット指定子（`%04d`や`%02d`）を使うことで、桁数や整数の0埋めなど、細かい設定ができます。

## 参考資料

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [sprintf() function](https://www.cplusplus.com/reference/cstdio/sprintf/)
- [Formatting Date and Time in Arduino](https://www.instructables.com/Formatting-Date-and-Time-in-Arduino/)