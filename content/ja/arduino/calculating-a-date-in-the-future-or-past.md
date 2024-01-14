---
title:    "Arduino: 「未来または過去の日付の計算」"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

＃＃なぜ
将来や過去の日付を計算することに取り組む理由を1-2文で説明します。

＃＃方法
「```Arduino ... ```」のコードブロック内にコーディングの例とサンプルの出力を示します。

＊例1：フォーマットされた日付を取得する方法

```
Arduino
void setup(){
  Serial.begin(9600); //シリアルモニターの初期化
  Date date(20, 9, 2021); //計算する日付を設定
  String formattedDate = date.format("yyyy/mm/dd"); //年/月/日の形式で日付を取得
  Serial.println(formattedDate); //シリアルモニターに出力
}

void loop(){
  //何もしない
}
```

出力：2021/09/20

＊例2：特定の日数を加算した日付を取得する方法

```
Arduino
void setup(){
  Serial.begin(9600); //シリアルモニターの初期化
  Date date(20, 9, 2021); //計算する日付を設定
  date.addDays(7); //日付に7日を加算
  String formattedDate = date.format("yyyy/mm/dd"); //年/月/日の形式で日付を取得
  Serial.println(formattedDate); //シリアルモニターに出力
}

void loop(){
  //何もしない
}
```

出力：2021/09/27

＃＃ディープダイブ
将来や過去の日付を計算するためには、まず現在の日付を取得し、その日付から加算または減算することが必要です。ArduinoのDateライブラリを使用することで、日付を計算しやすくなります。また、年、月、日の値を単独で取得することもできます。

＃＃関連リンク
- https://www.arduino.cc/reference/en/libraries/date/
- https://www.programmingelectronics.com/working-with-dates-on-the-arduino/