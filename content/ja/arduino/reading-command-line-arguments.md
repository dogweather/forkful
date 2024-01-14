---
title:    "Arduino: コマンドライン引数の読み込み"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

なぜcommand line argumentsを読むことについて学ぶ必要があるのでしょうか？この記事では、Arduinoプログラムの開発においてどのように役立つのかについて説明します。

## How To

Arduinoのプログラムでコマンドライン引数を読み取ることは、プログラムに柔軟性を与える重要な方法です。コマンドライン引数は、Arduinoモニターからの入力やシリアルモニターからのデータの送信など、さまざまな方法で取得することができます。

以下のコードを使用すると、Arduinoプログラムでコマンドライン引数を読み取ることができます。

```Arduino

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // シリアルポートがオープンするまで待機する
  }
  
  // 引数を読み取る
  while (Serial.available()) {
    int value = Serial.parseInt();
    Serial.print("読み取った引数： ");
    Serial.println(value);
  }
}

void loop() {
 
}

```

上記のコードでは、 `Serial.parseInt()` 関数を使用してコマンドライン引数を読み取り、 `while` ループを使って複数の引数を読み込むことができます。引数が読み取られると、シリアルモニターにメッセージが表示されます。

例えば、シリアルモニターに `5` という数字を入力し、エンターキーを押すと、以下のようなメッセージが表示されます。

```
読み取った引数： 5
```

このように、コマンドライン引数を読み取ることで、プログラムの挙動を動的に変化させることができます。

## Deep Dive

コマンドライン引数を使うと、Arduinoプログラムの開発においてさまざまな機能を実装することができます。例えば、動作モードの切り替えや特定のデバイスを制御するための識別子など、様々な用途に応じて引数を使い分けることができます。

また、Arduinoモニター以外にも、Bluetoothや無線モジュールを使って外部から引数を受け取ることも可能です。これにより、ユーザーからのリアルタイムの指示に応じてプログラムの挙動を変化させることができます。

ただし、コマンドライン引数を使う際には、引数が正しい形式で入力されているかどうかを検証することが重要です。無効な入力値がプログラムに渡されると、予期しない動作を引き起こす可能性があります。

## See Also

参考文献や関連リンクを以下にリストアップしておきます。

- [Arduino - Serial.parseInt()](https://www.arduino.cc/reference/ja/language/functions/communication/serial/parseint/)
- [Arduino Project Hub - Using Command Line Arguments with an Arduino Application](https://create.arduino.cc/projecthub/HonzaTyc/using-command-line-arguments-with-an-arduino-application-645eba)
- [Arduino Forum - Command Line Variable Input](https://forum.arduino.cc/t/command-line-variable-input/253829/3)