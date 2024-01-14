---
title:                "C#: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

あなたは過去や未来の日付を計算したい時がありますか？それは例えば、誕生日やイベントの日付を計算したい場合や、期限を設定する際に必要になるかもしれません。C#プログラミング言語を使うことで、簡単に日付の計算ができます。

## 方法

日付を計算するには、DateTimeクラスを使用します。まずは以下のコードを参照してください。

```C#
// 現在から100日後の日付を計算する
DateTime now = DateTime.Now;
DateTime futureDate = now.AddDays(100);
Console.WriteLine("100日後の日付は：" + futureDate);

// 現在から5年後の日付を計算する
DateTime fiveYearsLater = now.AddYears(5);
Console.WriteLine("5年後の日付は：" + fiveYearsLater);
```

上記のコードを実行すると、現在の日付から100日後や5年後の日付が計算され出力されます。DateTimeクラスには、他にもAddMonthsやAddHoursなどさまざまなメソッドがあり、様々な日付の計算が可能です。また、日付のフォーマットを指定することもできます。

## 深堀り

日付を計算する際には、プラスやマイナスで指定した期間を日付に加えることで計算しています。DateTimeクラスは、元の日付を変更するのではなく新しいインスタンスを返すため、元のデータを保持することができます。そのため、計算を繰り返すことができるのです。

さらに、DateTimeクラスにはParseやTryParseのメソッドもあり、文字列型の日付を変換してDateTime型の日付に変換することもできます。

## 参考リンク

- [Microsoft Docs：DateTime クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=netcore-3.1)
- [CODEGIRL：C#での日付計算の方法](https://www.codegirl.co.jp/the-way-to-calculate-dates-in-c/)
- [C# 入門：日付処理の基礎](https://www.c-sharpcorner.com/UploadFile/af66b7/date-time-in-C-Sharp/)

## 他の記事を見る

- [プログラミング初心者へのアドバイス](https://github.com/akicho8/blog-casual/blob/master/posts/newbies_advice.md)