---
title:                "Arduino: 未来や過去の日付を計算する"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

「なぜ」: 過去や未来の日付を計算することは、センサーの読み取りや制御に便利です。

## Why

Arduinoマイクロコントローラーのプログラムを作成する際、時には日付を計算する必要があります。これは、例えば日付に基づいてあるタイミングで特定の操作を行う場合に便利です。また、センサーの読み取りや制御においても、過去や将来の日付を計算することが役立ちます。今回のブログでは、Arduinoプログラミングにおける日付の計算方法についてご紹介します。

## How To

日付を計算するためには、Arduinoのライブラリである「DateTime」を使用します。まず、ライブラリのインポートをします。

```
#include <DateTime.h>
```

次に、DateTimeオブジェクトを作成し、計算を行いたい年・月・日などの日付を指定します。ここでは、2022年10月1日を指定した例を示します。

```
DateTime futureDate(2022, 10, 1);
```

次に、「now()」関数を使用して現在の日付を取得し、今から将来の日付までの日数を計算します。

```
DateTime currentDate = now();
int days = futureDate.daysTo(currentDate);
```

これで、現在の日付から2022年10月1日までの日数が計算されます。このように、未来の日付を計算することができます。同様に、過去の日付を計算する場合は「daysTo()」関数の順序を入れ替えることで求めることができます。

また、DateTimeオブジェクトを作成した後、「year()」、「month()」、「day()」などの関数を使用することで、任意の日付の年・月・日を取得することもできます。

## Deep Dive

ArduinoのDateTimeライブラリには、時間や曜日などを取得するための様々な関数が用意されています。さらに、その他の日付の演算も可能です。詳しい情報は、公式ドキュメントをご確認ください。

## See Also

- The official Arduino DateTime library documentation: https://github.com/PaulStoffregen/DateTime
- A tutorial on using DateTime in Arduino projects: https://www.instructables.com/DateTime-Library/
- 日付の計算に便利なArduinoライブラリまとめ: https://qiita.com/nazrin/items/491a1f3e8e404aa8b2c4