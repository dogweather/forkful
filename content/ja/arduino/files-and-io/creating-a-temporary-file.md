---
date: 2024-01-20 17:39:29.090351-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u3063\u3066\u30C7\u30FC\u30BF\u3092\
  \u4E00\u6642\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u7D99\u7D9A\u7684\u306B\u30C7\u30FC\u30BF\u3092\u51E6\u7406\u3057\u3066\u3001\
  \u305D\u308C\u3092\u4E00\u6642\u7684\u306B\u3069\u3053\u304B\u306B\u4FDD\u6301\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308B\u304B\u3089\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.482332-07:00'
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u3063\u3066\u30C7\u30FC\u30BF\u3092\
  \u4E00\u6642\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u7D99\u7D9A\u7684\u306B\u30C7\u30FC\u30BF\u3092\u51E6\u7406\u3057\u3066\u3001\
  \u305D\u308C\u3092\u4E00\u6642\u7684\u306B\u3069\u3053\u304B\u306B\u4FDD\u6301\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308B\u304B\u3089\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ファイルを作ってデータを一時保存すること。プログラムが継続的にデータを処理して、それを一時的にどこかに保持する必要があるから。

## How to: (方法)

Arduinoには標準的なファイルシステムライブラリがありません。しかし、SDカードを使って一時ファイルを作成・管理することができます。

```arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");

  myFile = SD.open("temp.txt", FILE_WRITE);
  
  if (myFile) {
    myFile.println("Temp data");
    Serial.println("Data written to temp.txt");

    myFile.close();
  } else {
    Serial.println("Error opening temp.txt");
  }
}

void loop() {
  // 継続処理はここに書く
}
```

Sample Output:
```
Initialization done.
Data written to temp.txt
```

## Deep Dive (深掘り)

元々、Arduinoの環境には、コンピュータのような本格的なファイルシステムはありませんでした。だから、外部のストレージ、特にSDカードが便利です。`SD`ライブラリを使ってSDカードにアクセスし、標準的なファイル操作が可能です。代替手段としては、EEPROMを用いる方法がありますが、書き込み回数に制限があります。一時ファイルは、センサーデータのバッチ処理やデバッグ情報の記録によく使われます。重要なのは、SDカードのライフサイクルを理解し、ファイルの開閉を適切に行うことです。

## See Also (関連情報)

- ArduinoのSDライブラリドキュメント: [Arduino - SD](https://www.arduino.cc/en/reference/SD)
- EEPROMの利用方法: [Arduino - EEPROM](https://www.arduino.cc/en/Reference/EEPROM)
