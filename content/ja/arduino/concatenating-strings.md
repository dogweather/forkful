---
title:    "Arduino: 文字列を連結する"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# なぜ？
文字列の結合を行う理由は何でしょう？Arduinoプログラミングをする際に文字列結合が必要になることがあります。例えば、センサーからのデータを表示する際に、固定のテキストと合わせて表示するために文字列を結合する必要があります。

## 方法
Arduinoでは、文字列結合のために```strcat()```という関数があります。この関数は、2つの文字列を結合し、新しい結合された文字列を返します。以下の例をご覧ください。

```Arduino
// 文字列の定義
char str1[20] = "Hello";
char str2[20] = "Arduino";

// 文字列の結合
char result[40];
strcat(result, str1);
strcat(result, str2);

// 結果の出力
Serial.println(result);

// 出力結果：HelloArduino
```

## ディープダイブ
文字列結合には、他にも便利な関数があります。例えば、```sprintf()```関数は、複数の変数を結合した文字列を作成することができます。また、Arduinoでは、文字列結合するための便利なライブラリがありますので、調べてみることもオススメです。

## 関連リンク
- [Arduino sprintf()関数の使用方法](https://qiita.com/taruwealth/items/3165f45fefac2da8ebba)
- [Arduinoプログラミング入門 - 文字列編](https://www.arduino.cc/en/Tutorial/String)
- [Arduinoプログラミングにおける配列とポインタの扱い方](https://www.petitmonte.com/robot/howto_string.html)