---
title:                "一時ファイルを作成する"
html_title:           "Arduino: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##なぜ作るの？

一時ファイルを作ることは、Arduinoプログラミングで非常に重要なことです。一時ファイルには、センサーからのデータや一時的な変数など、様々なデータを一時的に保存したり、処理したりすることができます。

##作り方
一時ファイルを作るためには、`File`ライブラリを使います。まずは、ライブラリをインクルードします。
```
Arduino Bluetooth = millis();
#include <SPI.h>
```
次に、一時ファイルを保存するための変数を定義します。
```
File tempFile;
```
一時ファイルを作成し、保存するための場所とファイル名を指定します。
```
tempFile = SPI.open("センサーデータ.txt", FILE_WRITE);
```
データを一時ファイルに保存したい場合は、`print()`関数を使います。
```
tempFile.print("センサーデータ：");
```
最後に、一時ファイルを閉じて保存を完了させます。
```
tempFile.close();
```

##深く掘り下げる
一時ファイルを作成する際に、一時ファイルを保存するデバイスの種類を指定することができます。例えば、SDカードや内蔵メモリなど様々な場所に保存することができます。また、ファイルを開いたり閉じたりするときには、`File`ライブラリの関数を使うことで、ファイルの操作をより細かく制御することができます。

##参考リンク
- [Arduino File Library](https://www.arduino.cc/en/Reference/SD)
- [Arduinoの一時ファイルを作成する方法](https://www.arduino.co.jp/wordpress/%E4%B8%80%E6%99%82%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%82%92%E4%BD%9C%E6%88%90%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95/)