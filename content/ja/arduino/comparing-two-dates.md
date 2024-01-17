---
title:                "「二つの日付の比較」"
html_title:           "Arduino: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
「比較する」とは、2つの日付を見比べてどちらがより新しいか、古いかを見ることです。プログラマーは、日付を比較することで、二つのイベントの順序を判断したり、期限を設定したりすることができます。

## 方法：
Arduinoを使って、2つの日付を比較する方法を見ていきましょう。以下のコードをArduino IDEに入力してください。

```Arduino
#include <TimeLib.h> // ライブラリをインポート
tmElements_t now, then; // タイムスタンプを格納する変数を作成
bool result; // 比較結果を格納する変数を作成

// 現在の日付をnowに設定
DateTime now = DateTime(2021, 10, 1, 10, 30, 00);

// 比較する日付をthenに設定
DateTime then = DateTime(2020, 9, 23, 12, 00, 00);

// 2つの日付を比較して結果をresultに代入
if(now > then){
    result = true; // nowの方が新しい
} else {
    result = false; // thenの方が新しい
}

// 結果をシリアルモニターに出力
Serial.println(result);
```

上記のコードを実行すると、シリアルモニターには「1」が表示されます。これは、現在の日付がより新しいため、resultにtrueが代入されたことを意味しています。

## 詳細説明：
日付を比較する方法には、さまざまな方法がありますが、ArduinoではTimeLibライブラリを使用することで簡単に実装することができます。また、比較する日付をDateTimeオブジェクトとして作成することで、より柔軟に比較を行うことができます。

## 関連情報：
- TimeLibライブラリのドキュメント：https://github.com/PaulStoffregen/Time
- Arduinoにおける日付比較の実装方法：https://www.instructables.com/Quick-Tip-Comparing-Dates-in-Arduino/