---
title:                "Arduino: 「テキストファイルの書き方」"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

こんにちは、Arduinoプログラミングの愛好家の皆さん！今回は、テキストファイルを書くための方法についてお話ししようと思います。

## Why

テキストファイルは、データを保存したり情報を共有するために非常に便利な方法です。あなたのArduinoプロジェクトでも、センサーデータや設定情報などをテキストファイルとして保存できます。また、テキストファイルを読み取ることで、機能的なアプリケーションを作成することもできます。

## How To

テキストファイルを書くためには、まず`SD`ライブラリを使用する必要があります。次のようにライブラリをインクルードします。

```Arduino
#include <SD.h>
```

次に、SDカードを初期化し、テキストファイルを作成します。

```Arduino
SD.begin(10); // 10はSDカードのCSピン番号
File myFile = SD.open("myFile.txt", FILE_WRITE); // "myFile.txt"はファイル名, FILE_WRITEは書き込むモード
```

テキストファイルを書き込むには、`File`オブジェクトの`print()`や`println()`メソッドを使用します。例えば、次のように文字列をファイルに書き込むことができます。

```Arduino
String message = "Hello world!";
myFile.println(message);
```

書き込みが終わったら、ファイルを閉じてSDカードから取り外します。

```Arduino
myFile.close();
SD.remove("myFile.txt");
SD.end();
```

これで、テキストファイルを作成・書き込み・保存することができます。

## Deep Dive

テキストファイルを作成する際には、`SD.open()`メソッドの第二引数に書き込みモードだけでなく読み取りモードも指定することができます。また、`File`オブジェクトの`read()`や`available()`メソッドを使用してファイルからデータを読み取ることもできます。詳細な情報は、[公式ドキュメント](https://www.arduino.cc/reference/en/libraries/sd/)をご覧ください。

See Also

- [Arduino SD Library Reference](https://www.arduino.cc/reference/en/libraries/sd/)
- [Arduino SD Card Tutorial for Beginners](https://randomnerdtutorials.com/complete-guide-for-ultrasonic-sensor-hc-sr04/)