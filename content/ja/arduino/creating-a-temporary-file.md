---
title:    "Arduino: 一時ファイルの作成"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##なぜArduinoプログラミングをするのか

Arduinoプログラミングは、ユーザーが自分のアイデアを実現するために使われます。アイデアを実現するためには、しばしば一時的なファイルを作成する必要があります。このブログポストでは、一時的なファイル作成の方法について説明します。

##作り方

一時的なファイルを作成するには、まず`tmpFile`というファイルオブジェクトを定義します。次に、`SD.open()`関数を使用して、そのファイルを読み取り書き込み用に開きます。`tmpFile`にデータを書き込みたい場合は、`tmpFile.write()`関数を使います。最後に、`tmpFile.close()`関数を使用してファイルを閉じます。

```Arduino
File tmpFile; // ファイルオブジェクトを定義
tmpFile = SD.open("temporary.txt", FILE_WRITE); // 一時的なファイルを開く
tmpFile.write("Hello World!"); // テキストを書き込む
tmpFile.close(); // ファイルを閉じる
```

このコードを実行すると、SDカードに`temporary.txt`というファイルが作成され、そこに`Hello World!`というテキストが書き込まれます。

##深堀り

一時的なファイルを作成するには、必ずしもSDカードを使用する必要はありません。ArduinoのボードにはEEPROMと呼ばれるデータ保存用のメモリが備わっています。このEEPROMを使用して、一時的なデータの保存が可能です。EEPROMへの書き込み方法はSDカードと同様ですが、SDカードよりも高速であることが特徴です。

##もっと詳しく知りたい方は

- [SD Library - Arduino Reference](https://www.arduino.cc/en/Reference/SD)
- [EEPROM Library - Arduino Reference](https://www.arduino.cc/en/Reference/EEPROM)
- [Arduino EEPROM - Programming Electronics Academy](https://www.programmingelectronics.com/read-write-eeprom-arduino/)