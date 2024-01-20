---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## それは何で、なぜ？（What & Why?）
新プロジェクトの開始は、新しく土台からソフトウェアを作り出すことを指します。プログラマーがこれを行う理由は、特定の問題を解決するために、全く新しいアプローチが必要だからです。

## どうやって：（How to:）
Arduinoの新プロジェクトを始める最初のステップは、セットアップとループ関数の定義です。以下にサンプルコードを示します。

```Arduino
void setup() {
  // 初期化コード
}

void loop() {
  // 繰り返しコード 
}
```
このセットアップ関数では一度だけ実行され、ハードウェアの初期化などに使います。一方、ループ関数は何度も繰り返され、主なロジックを置きます。

## ディープダイブ（Deep Dive）
開始新プロジェクトは、必要性や解決すべき問題に応じてかなり異なる形状を取ることがあります。Arduinoの初のリリースは2005年で、以来DIYersやプロのエンジニアによって幅広く使われています。代替としてRaspberry PiやESP32などの他のマイクロコントローラを選択することもできます。

Arduino IDE(統合開発環境)で新プロジェクトを開始する際には、まず**ファイル > 新規作成**を選択します。この操作で新しいスケッチが作成され、上記の例のように`setup`と`loop`関数が自動で含まれます。

## 参考にしてください（See Also）
Arduino公式サイトのチュートリアルは非常に参考になります: [Arduinoチュートリアル](https://www.arduino.cc/en/Tutorial/HomePage)

さらなる詳細に興味がある場合は、以下のリンクも参照してください:
- [Arduinoプログラミングシンタックス](https://www.arduino.cc/reference/en/)
- [Raspberry Pi公式サイト](https://www.raspberrypi.org/)
- [ESP32公式サイト](https://www.espressif.com/en/products/socs/esp32)