---
title:                "Arduino: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認する理由は、プログラマーがファイルが存在するかどうかを判断し、必要に応じて処理を実行するためです。

## How To
ディレクトリが存在するかどうかを確認するには、`File` クラスの `exists()` メソッドを使用します。以下の例では、ディレクトリが存在するかどうかを確認し、結果をシリアルモニターに出力する方法を示します。

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  
  // SDカードの初期化
  if (!SD.begin(10)) {
    Serial.println("SDカードの初期化に失敗しました！");
  }
  
  // ディレクトリのパスを指定
  File dir = SD.open("/my_directory");
  
  // ディレクトリが存在するかどうかを確認
  if (dir.exists()) {
    Serial.println("ディレクトリが存在します！");
  } else {
    Serial.println("ディレクトリが存在しません！");
  }
  
  // ディレクトリを閉じる
  dir.close();
}

void loop() {
  // ここに追加の処理を記述することもできます
}
```

上記のコードを実行すると、SDカードに `my_directory` というディレクトリが存在するかどうかが確認され、結果がシリアルモニターに出力されます。

## Deep Dive
`File` クラスの `exists()` メソッドは、指定したファイルまたはディレクトリが存在する場合に `true` を返し、存在しない場合に `false` を返します。このメソッドを使用することで、ファイルが存在するかどうかを簡単に判定することができます。

ただし、このメソッドは常に `true` または `false` を返すため、ファイルが読み取り可能かどうかやアクセス権限などの詳細な情報は取得できません。より詳細な情報を取得するには、`File` クラスの他のメソッドを使用する必要があります。

## See Also
- [File Class Reference](https://www.arduino.cc/en/Reference/SDFile)
- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino File System Tutorial](https://www.arduino.cc/en/Tutorial/ReadWrite)