---
title:                "Arduino: 日付を文字列に変換する"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する必要があるかもしれません。例えば、LCD画面に日付を表示したり、データをログに記録したりする際に、文字列形式で日付を扱う必要があります。Arduinoを使う場合、どのように日付を文字列に変換するかを学ぶことが重要です。

## 方法
まずは、日付を取得する方法を確認しましょう。Arduinoでは、`DateTime`ライブラリを使うことで、現在の日付や時間を取得することができます。次に、取得した日付を文字列に変換する方法を見ていきます。

```
Arduino ...

#include <DateTime.h>
DateTime now = DateTime.now();

// 日付を取得する
int year = now.year();
int month = now.month();
int day = now.day();

// 日付を文字列に変換する
String date = String(year) + "/" + String(month) + "/" + String(day);

```

この方法では、取得した日付を`String`型にキャストし、`+`演算子を使って文字列を結合しています。このようにすることで、日付を文字列として扱えるようになります。

さらに、フォーマットを変更したい場合は、`DateTime`ライブラリに含まれる`toString()`関数を使うことで日付のフォーマットを指定することができます。例えば、`toString("YYYY-MM-DD")`とすると、年月日の順に表示されるようになります。

## 深堀り
日付を文字列に変換する方法はさまざまありますが、一般的な方法としては、`sprintf()`関数を使うことが挙げられます。この関数は、指定したフォーマットに従って文字列を生成することができます。例えば、`sprintf(output, "%04d/%02d/%02d", year, month, day)`とすると、`output`には`YYYY/MM/DD`形式で日付が生成されるようになります。

また、日付を日本語表記にしたい場合は、外部ライブラリの`TimeLib`を使うことで可能です。このライブラリでは、`Japanese_Holidays.h`を追加することで、日本の祝日を日本語で表示することができます。

## 参考リンク
- [DateTime Library by Makuna](https://github.com/Makuna/DateTime)
- [sprintf() function](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)
- [Time Library by PaulStoffregen](https://github.com/PaulStoffregen/Time)
- [Japanese Holidays Library](https://github.com/iwatake2222/Japanese_Holidays)