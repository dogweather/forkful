---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力の印刷は、コードにバグがある場合にその原因を特定する手助けとなる情報を出力する方法です。プログラマーがこれを行う主な理由は、問題を特定し、解決策を見つけるためです。

## どうやったら

以下に、Arduinoでデバッグ出力を印刷する基本的なコードを示します。

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = analogRead(A0);
  Serial.println(sensorValue);
  delay(100);
}
```

以上のコードは、A0ピンのアナログ値を読み取り、その値をシリアルポートに出力します。

以下のような出力が表示されるはずです:

```
1134
1156
1127
1154
```

これは、センサーの値が時間経過とともに変動していることを示しています。

##深なる探求

デバッグ出力の印刷は古くから存在するもので、ソフトウェア開発の初期からあります。代替方法としては、特定のアプリケーションでログファイルを作成することや、デバッガツールを使用することがあります。Arduinoでは、シリアル通信を使ってデバッグ情報をPCに送信するという実装がなされています。

## その他参照

Arduinoでより高度なデバッグを行う方法については次のリンクをご覧ください: