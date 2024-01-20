---
title:                "文字列から日付を解析する"
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?
文字列から日付を解析するとは、文字列形式の日付をプログラムに認識可能な日付形式に変換することです。このプロセスは、ユーザーが日付情報を入力し、それをプログラムで処理する必要があるアプリケーションで頻繁に行われます。

## どうやって:
以下に、Arduinoで文字列から日付を解析する例を示します。

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  char dateString[] = "5/05/2022";
  TimeElements tm;
  if (parseTime(dateString, tm)) {
    Serial.print("Year: ");
    Serial.println(tm.Year + 1970); 
    Serial.print("Month: ");
    Serial.println(tm.Month);
    Serial.print("Day: ");
    Serial.println(tm.Day);
  } else {
    Serial.println("Failed to parse date");
  }
  delay(1000);
}
```

上記のコードの出力は以下のようになります。

```
Year: 2022
Month: 5
Day: 5
```

## ディープダイブ
日付解析の歴史は古く、その起源はプログラムが日付情報を理解して処理する必要があるときにまで遡ります。替代案として、一部の開発者はUNIXタイムスタンプを直接使用しますが、これは人間にとっては可読性が低くなります。Arduinoの`parseTime`関数は、内部的には文字列を区切り記号で分割し、それぞれの部分を適切な日付フィールドに格納する単純な方法を採用しています。

## 関連資料
以下のリンクは、このトピックに関してより深く理解するためのものです。

1. Arduinoの公式文書: [Time Library](https://www.arduino.cc/reference/en/libraries/time/)
2. [Parsing date and time from serial input](https://forum.arduino.cc/index.php?topic=136447.0) 
3. [Date and Time functions](http://playground.arduino.cc/code/time)

記事を読んでいただきありがとうございます。