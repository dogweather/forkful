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

## この記事は何のためにあるの? 
Arduinoの最新バージョンを使ったプログラミングの方法を教える記事だよ。特に、「日付を文字列に変換する方法」について詳しく説明するよ。プログラマーたちは、日付を文字列に変換することができると、データの処理や表示がより簡単になるから、この方法を使うんだ。

## どうやってやるの? 
以下のコードをArduinoソフトウェアにコピーして、マイクロコントローラーに書き込むことで、日付を文字列に変換することができるよ！また、変換した日付をシリアルモニターに表示させることもできるよ。

```Arduino
// 日付を取得
int day = day(); // 日
int month = month(); // 月
int year = year(); // 年

// 日付を文字列に変換
String date = String(day) + "/" + String(month) + "/" + String(year);

// シリアルモニターに表示
Serial.println("今日の日付は" + date + "です。");
```

表示される結果は以下のようになるよ！
今日の日付は25/12/2020です。

## さらに詳しく 
### 歴史的背景 
もともと、日付を文字列に変換するためには、プログラマーが手作業で数字を文字に変える必要がありました。しかし、Arduinoのようなマイクロコントローラーの登場により、日付を文字列に変換する方法も簡単になりました。

### 他の方法 
日付を扱う上で、文字列に変換する方法以外にも、さまざまな手法があります。例えば、日付を数値として取得し、計算や比較を行うことでも、日付の処理ができます。

### 実装の詳細 
Arduinoでは、```String```というデータ型を使用することで、文字列を扱うことができます。この記事のコードでは、日付を文字列に直す際に、```String```を使用しています。

## 関連情報を見てみよう 
- [公式Arduinoチュートリアル: 文字列の操作](https://www.arduino.cc/en/Tutorial/StringConstructors)
- [Arduinoリファレンス: day(), month(), year()](https://www.arduino.cc/reference/en/language/functions/time/day/)
- [日付を扱うためのC/C++標準ライブラリ](https://www-use.cs.york.ac.uk/supple/software/qn_anon/trol/doc/mirrors/c_cpp_lib/c_time.html)