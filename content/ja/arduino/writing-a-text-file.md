---
title:    "Arduino: テキストファイルの作成"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

#なぜ
Arduinoプログラミングをする理由は何でしょうか？これまで私たちが作ってきたプロジェクトは、LEDやモーターなどの電子部品を使用してきましたが、テキストファイルを書くことができれば、さらに多くのことができるようになります。例えば、センサーからのデータを保存することで、後から確認することができたり、プログラムの設定を簡単に変更することができたりします。

##方法
Arduinoでテキストファイルを書く方法を紹介します。まずはファイルを作成するためのライブラリをインストールします。例えば、SDカードモジュールを使用する場合は、以下のようなコードを使います。

```Arduino
#include <SPI.h>
#include <SD.h>

//ピンの指定
#define SD_ChipSelectPin 4

void setup() {
  //SDカードと通信するための初期化
  Serial.begin(9600);
  while(!Serial) {
    //シリアルポートが開いていない場合は待機
  }

  //SDカード初期化
  Serial.print("Initializing SD card...");
  if(!SD.begin(SD_ChipSelectPin)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
}

void loop() {
  //ファイルの作成
  File dataFile = SD.open("data.txt", FILE_WRITE);
  
  //ファイルにデータを書き込む
  dataFile.println("Hello World!");
  
  //ファイルを閉じる
  dataFile.close();
}
```

上記のコードを実行すると、SDカード内にdata.txtというファイルが作成され、その中に"Hello World!"という文字列が書き込まれます。他の方法として、シリアルモニター上でテキストを入力し、その内容をファイルに書き込むこともできます。

##ディープダイブ
Arduinoでテキストファイルを書くには、SDカードやEEPROMといった外部のメモリーを利用する必要があります。これらのメモリーは、電源を切ってもデータが保持されるため、データの永続性が必要なプログラムにはよく利用されます。また、ファイルの読み書きをするには、ファイルポインターの管理や文字列の処理といったプログラミングの知識が必要になります。

##次はこちらをご覧ください
- [Arduino SDライブラリーの使い方](https://www.arduino.cc/en/Reference/Library)
- [Arduino EEPROMライブラリーの使い方](https://www.arduino.cc/en/Reference/EEPROM)
- [Arduinoファイル入出力の基本](http://www.microsmart.co.jp/microbit/learn/arduino/basic_file/)