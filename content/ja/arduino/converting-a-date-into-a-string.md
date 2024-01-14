---
title:    "Arduino: 「日付を文字列に変換する」"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する作業は、Arduinoプログラミングにおいて非常に重要です。日付を文字列に変換することで、データの表示や保存、さまざまなアプリケーションの作成など、様々な用途に役立てることができます。

## 方法

まずは、日付を取得する方法を学びましょう。ArduinoのライブラリであるTimeを使用することで、現在の日付を簡単に取得することができます。以下のコードをArduino IDEに入力して、シリアルモニタを開いて実行してみてください。

```Arduino
#include <Time.h>

// 現在の日付を取得する
time_t now = time(nullptr);

// シリアルモニタに現在の日付を表示する
Serial.println(now);

```

実行すると、1970年1月1日からの秒数が表示されるはずです。この秒数をUnixエポックと呼びます。では、このUnixエポックを日付に変換するにはどうすればよいでしょうか。そのためには、`timeToBuffer()`関数を使用します。

```Arduino
// 日付を格納するための配列を作成する
char buffer[20];

// Unixエポックを日付に変換する
timeToBuffer(now, buffer);

// シリアルモニタに日付を表示する
Serial.println(buffer);

```

実行すると、現在の日付が表示されるはずです。ただし、日付の形式は数字の羅列であまり見栄えがよくありません。そこで、日付を文字列に変換する方法を紹介します。

```Arduino
// 日付のフォーマットを指定する
String dateFormat = "yyyy/MM/dd";

// 日付を文字列に変換する
String strDate = String(now, DEC);

// 日付のフォーマットに合わせる
strDate.replace("yyyy", strDate.substring(0, 4));
strDate.replace("MM", strDate.substring(4, 6));
strDate.replace("dd", strDate.substring(6, 8));

// 文字列を表示する
Serial.println(strDate);

```

これで、見やすい形式の日付が表示されるはずです。

## ディープダイブ

日付を文字列に変換する方法について、より詳しく説明しましょう。まずはUnixエポックとは何かを理解することが重要です。Unixエポックとは、1970年1月1日00:00:00からの経過秒数を表すものです。これを利用することで、現在の日付や時間を扱うことができます。

また、`timeToBuffer()`関数についても詳しく見ていきましょう。この関数は、引数としてUnixエポックと日付を格納する配列を受け取り、Unixエポックを指定の日付形式に変換して格納します。この関数を使用することで、日付や時間をさまざまな形式に変換することができます。

## その他の記事

- [Arduino公式サイトのTimeライブラリ](https://www.arduino.cc/en/Reference/Time)
- [C言語の時間関数についての解説 (英語)](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Timeライブラリのドキュメント (英語)](https://github.com/PaulStoffregen/Time)
- [日付を文字列に変換する