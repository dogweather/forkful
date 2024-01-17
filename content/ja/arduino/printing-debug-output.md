---
title:                "「デバッグ出力のプリント」"
html_title:           "Arduino: 「デバッグ出力のプリント」"
simple_title:         "「デバッグ出力のプリント」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何をするのか & なぜするのか？
デバッグ出力とは何かを理解する前に、なぜプログラマーがそれを行うのかを知ることが重要です。デバッグ出力は、コードが実行される過程で起こったことを確認するために使われます。プログラマーは、コードの実行中に発生するエラーや値の変化を把握し、問題を解決するのに役立てます。

## 方法：
```Arduino
// デバッグ出力の例
int x = 5;
Serial.println("xの値は: ");
Serial.println(x);
```
上記のコードを実行すると、シリアルモニターに以下のように表示されます：
```
xの値は:
5
```
これにより、プログラムが実行される際にxの値がどのように変化するかを確認することができます。また、変数の値を表示するだけでなく、コードのどの部分が実行されているかを確認することもできます。

## 詳細を掘り下げる：
デバッグ出力は、プログラミングにおいて重要な役割を果たしています。昔のプログラマーたちは、コンソールにメッセージを表示することでデバッグを行っていましたが、現在ではシリアルモニターを使用することが一般的です。デバッグ出力の代わりにデバッガーを使用することもできますが、その場合はハードウェアが必要になります。デバッグ出力は、シンプルかつ効果的な方法でコードの問題を特定することができます。

## 関連情報：
- [Arduino公式ウェブサイト](https://www.arduino.cc/reference/jp/language/functions/communication/serial/println/)
- [デバッグ出力の重要性について知る](https://www.youtube.com/watch?v=6hf1Y6q0XI4)
- [他のデバッグ方法について学ぶ](https://www.arduino.cc/en/Tutorial/Debugging)