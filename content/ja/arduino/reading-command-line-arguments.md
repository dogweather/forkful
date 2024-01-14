---
title:    "Arduino: コマンドライン引数の読み取り"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# なぜ
コマンドライン引数を読み込むことについて学ぶ理由は、あなたのプログラムに動的な要素を追加することができるからです。

##方法
コマンドライン引数を読み込むためには、Arduinoプログラミングのコードを使用する必要があります。以下は、コマンドライン引数を読み込むための基本的なコード例です。 

```Arduino
void setup() {
  Serial.begin(9600); //シリアル通信を開始する
  while(!Serial);

  //コマンドライン引数の数を取得する
  int argc = CommandLine.arguments();
  Serial.print("引数の数： ");
  Serial.println(argc);

  //各引数を表示する
  for(int i = 0; i < argc; i++) {
    String arg = CommandLine.getArgumentAt(i);
    Serial.print("引数 ");
    Serial.print(i);
    Serial.print(": ");
    Serial.println(arg);
  }
}

void loop() {
  //何もしない
}
```

上記のコードを実行すると、シリアルモニターにコマンドライン引数の数と各引数が表示されます。

##ディープダイブ
コマンドライン引数を読み込むと、プログラムの実行前に、使用者が引数を指定できます。これにより、プログラムの挙動をカスタマイズすることができます。また、引数を使用することで、プログラムの再利用性が高まります。

###注意点
コマンドライン引数を使用する際には、引数の型に注意する必要があります。コマンドライン引数はすべて文字列として読み込まれますので、必要に応じて数字などの別の型に変換する必要があります。

##見てみる
- [Arduinoのコマンドライン引数の読み込み方](https://github.com/arduino/ArduinoCore-avr/issues/247)
- [コマンドライン引数を使用したArduinoプログラムの例](https://bitbucket.org/kbsigma/arduino-command-line-arguments/src/master/)