---
title:                "Arduino: コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読み込みコマンドライン引数をするのか

コマンドライン引数を読み込むことで、Arduinoプログラミングにおいてより効率的にコードを実行することができます。また、ユーザーが入力した情報を取得することも可能になります。

## 読み込みコマンドライン引数の方法

Arduinoでは、```void setup()```内に```Serial.begin(9600);```と入力することで、シリアルポートを開き、モニターに情報を表示することができます。次に、```void loop()```内に```if(Serial.available()>0){}```を記述し、中括弧内に引数を読み込むコードを記述します。最後に、モニターに表示させたい情報に対する処理を追加することで、コマンドライン引数を読み込んでそれを使用することができます。例えば、読み込んだ情報を変数に代入してから、その変数をモニターに表示するなどの処理が可能です。

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available() > 0) {
    int arg1 = Serial.parseInt(); // 引数を整数として読み込む
    float arg2 = Serial.parseFloat(); // 引数を浮動小数点数として読み込む
    String arg3 = Serial.readString(); // 引数を文字列として読み込む
    // 引数を使用した処理
    Serial.println(arg1 + arg2); // 読み込んだ引数の足し算をモニターに表示
    Serial.println(arg3); // 読み込んだ引数をそのままモニターに表示
  }
}
```

## コマンドライン引数の深層

コマンドライン引数を読み込むことで、ユーザーが入力した情報を取得し、それを使用することができます。この機能を活用することで、ユーザーとのやり取りや、特定の条件に応じた処理を実行することが可能になります。また、コマンドライン引数を複数指定することもでき、それらを別々の変数に割り当てることで、より複雑な処理を行うことができます。

### 番外編：ライブラリの活用

さらに、コマンドライン引数を取得するためのライブラリも存在します。例えば、Arduino CLIやCmdParserなどがあり、これらを使用することで、より簡単にコマンドライン引数を取得することができます。ただし、ライブラリを使用する場合は、事前にインストールが必要です。

## 関連リンク

- [Arduino CLI](https://github.com/arduino/arduino-cli)
- [CmdParser](https://github.com/roboremo/CmdParser)