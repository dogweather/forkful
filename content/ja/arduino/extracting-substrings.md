---
title:                "Arduino: 部分文字列の抽出"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

Arduinoは、ユーザーが独自のプログラムを作成して、マイクロコントローラーを制御することができるプログラミングツールです。今回は、Arduinoを使って文字列から部分文字列を抽出する方法について紹介します。これは、文字列処理を行う上で重要なスキルであり、多くのアプリケーションで役立ちます。

## 方法

Arduinoで部分文字列を抽出する方法は簡単です。まずは、抽出したい文字列を定義します。次に、```substring()```関数を使用して、抽出したい部分文字列の開始位置と終了位置を指定します。例えば、文字列 "Arduinoの部分文字列を抽出する方法" から "抽出する方法" を抽出するコードは以下のようになります。

```
Arduinoの部分文字列を抽出する方法

String str = "Arduinoの部分文字列を抽出する方法";

String substr = str.substring(11, 18);

Serial.println(substr); //出力結果: 抽出する方法
```

この例では、```substring()```関数の引数として、開始位置を11、終了位置を18と指定しています。これにより、元の文字列の11番目の文字から18番目の文字までが抽出され、```substr```に代入されます。

## ディープダイブ

文字列を操作する上で部分文字列の抽出は重要な方法です。例えば、センサーやデバイスから送られてくる文字列の一部を取り出して特定の値を抽出することができます。また、文字列を分割して特定の単語や数値を入力する際にも部分文字列の抽出が役立ちます。Arduinoでは、```indexOf()```関数や```subString()```関数以外にもさまざまな文字列操作関数が用意されています。これらを組み合わせることで、より複雑な文字列処理を行うことができます。

## 参考リンク

- [Arduino公式サイト](https://www.arduino.cc/)
- [Arduino リファレンス](https://www.arduino.cc/reference/)
- [Arduinoで文字列を操作する方法](https://qiita.com/mamoruuuuuuuuuu/items/a14432a05b052f825eee)
- [Arduinoで部分文字列を抽出する方法](https://en-in.instructables.com/id/Extract-Substring-of-a-String-On-Arduino/)