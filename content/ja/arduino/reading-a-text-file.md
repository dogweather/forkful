---
title:                "Arduino: テキストファイルを読む"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

### なぜ

Arduinoプログラミングに興味のある方なら、テキストファイルを読み込むことはとても役に立つスキルです。例えば、センサーのデータを保存しておくため、またはプログラムの設定を簡単に変更するために、テキストファイルで設定変数を保存することができます。

### 方法

まず、テキストファイルを読み込むためには、`SD`ライブラリを使用する必要があります。コードの始めに、次のように記述してライブラリをインポートします。

```Arduino
#include <SD.h>
```

次に、SDカードを初期化します。

```Arduino
if (!SD.begin(10)) {
    Serial.println("SDカードが見つかりませんでした。");
    return;
}
```

これで、SDカードからの読み込みが可能になりました。次のコードは、テキストファイルを1行ずつ読み込み、シリアルモニターに出力する例です。

```Arduino
File myFile = SD.open("myfile.txt", FILE_READ);
if (myFile) {
    while (myFile.available()) {
        Serial.println(myFile.readStringUntil('\n'));
    }
    myFile.close();
}
```

### 深堀り

テキストファイルを読み込むためには、SDカード内のファイルパスを指定する必要があります。上記の例では、`"myfile.txt"`と書いていますが、必要に応じてファイル名を変更することができます。

また、`readStringUntil()`関数は、指定した文字までの文字列を読み込みますが、他の関数を使用してファイルから読み込むことも可能です。さらに、テキストファイル以外のファイルも読み込むことができます。詳細は[SDライブラリの公式ドキュメント](https://www.arduino.cc/en/Reference/SD)を参照してください。

### 参考リンク

- [SDライブラリの公式ドキュメント](https://www.arduino.cc/en/Reference/SD)
- [ArduinoでSDカードを使用する方法](https://amba.to/2QDUDA6)