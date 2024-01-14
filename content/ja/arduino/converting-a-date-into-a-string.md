---
title:    "Arduino: 日付を文字列に変換する"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

日本語読者の皆さん、こんにちは！今日は、Arduinoプログラミングのトピックについてお話ししましょう。特に、日付を文字列に変換する方法について、学びたい方はいらっしゃいますか？そうであれば、この記事がお役に立てるかもしれません。では早速、見ていきましょう！

## なぜ日付を文字列に変換するのか

日付を文字列に変換する理由はさまざまです。例えば、センサーデータを収集している場合、取得した日付を記録したいときがあります。また、ディスプレイに日付を表示したいときなども、文字列に変換する必要があります。日付を扱う機会があるかもしれませんので、この方法は覚えておくと便利ですよ！

## 方法

では、実際に日付を文字列に変換する方法を見ていきましょう。以下のコードを参考にしてください。

```Arduino
#include <DateTime.h>

void setup() {
    // 現在の日付と時刻を取得
    DateTime now = DateTime.now();
    
    // 日付を文字列に変換
    char date_str[20]; // 文字列を格納するための配列を作成
    sprintf(date_str, "%d/%d/%d", now.year(), now.month(), now.day()); // フォーマット指定して日付を文字列に変換
    Serial.println(date_str); // シリアルモニターに出力
}

void loop() {
    // 何か処理を行う
}
```

上記のコードでは、DateTimeライブラリを使用して、現在の日付を取得しています。そして、sprintf関数を使用して、指定したフォーマットに従って、日付を文字列に変換しています。最後に、Serial.println関数を使って、文字列を表示しています。

この方法を参考に、自分のプロジェクトで日付を文字列に変換してみてください！

## 詳しく見ていく

DateTimeライブラリを使って日付を取得する方法は、上記のコードのように簡単です。ただし、注意しなければいけない点があります。DateTime.now()関数を呼び出す際に、事前にDateTimeライブラリをインクルードする必要があります。また、使用するマイクロコントローラによって、使用できるDateTimeライブラリが異なる場合があるので、事前に確認してください。

さらに、sprintf関数では、日付だけでなく、時刻を文字列に変換することもできます。詳しい使い方については、Arduinoの公式サイトやコミュニティで調べてみてください。

## See Also（関連リンク）

- Arduino公式サイト - https://www.arduino.cc/
- DateTimeライブラリの使い方 - https://www.arduino.cc/reference/en/libraries/datetime/
- sprintf関数の使い方 - https://www.cplusplus.com/reference/cstdio/sprintf/ 

以上が、日付を文字列に変換する方法についての紹介でした。日付を扱う機会があるかもしれませんので、ぜひ覚えておいてくださいね。それでは、楽しいArduinoプログラミングを！