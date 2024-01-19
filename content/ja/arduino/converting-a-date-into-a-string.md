---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Arduinoでの日付を文字列に変換方法
あなたのプロジェクトに日付を使用していますか？それとも、Arduinoで時間スタンプを利用したいですか？それなら、この記事はあなたのためです。

## 何となぜ？
日付を文字列に変えるとは、日付を「2021/12/01」のような形式の文字列に変える手法を指します。これは、日付を表示したり、日付に基づいたデータをログとして保存したりするためにプログラマが行うものです。

## 実施方法：
Arduinoエコシステムでは、「**sprintf**」関数を使用して日付を文字列に変換するための一貫した方法が提供されています。以下に簡単なコード例を示します： 

```Arduino
char dateStr[11]; // 'YYYY/MM/DD\0'
sprintf(dateStr, "%04d/%02d/%02d", year, month, day);

// 出力: 2021/12/01
Serial.println(dateStr);
```

## ディープダイブ：
- **歴史的背景**：初期のプログラミング言語では、データタイプの変換機能は限定的で、日付から文字列への変換は一般的ではありませんでした。しかし、時間の経過とともに、この機能が一般的になり、現在では多くのプログラミング言語がこの機能をサポートしています。
- **代替手段**：Arduinoでは、別のライブラリーを使って日付を文字列に変換することもできます。例えば、「**TimeLib**」や「**RTClib**」などがあります。
- **実装の詳細**：上記のコード例では、「sprintf」関数を使って日付を「YYYY/MM/DD」形式の文字列に変換しています。ここで言う「%04d」や「%02d」は、整数を4桁や2桁の文字列に変換する書式指定子です。

## 参照資料：
1. Arduino `sprintf`関数のドキュメンテーション: [Arduino sprintf](https://www.arduino.cc/reference/en/language/functions/characters/sprintf/)
2. 日付を文字列に変換する他の手法: [Using Time Library](https://playground.arduino.cc/code/time)

この記事があなたのプロジェクトに役立つことを願っています。幸運を祈ります！