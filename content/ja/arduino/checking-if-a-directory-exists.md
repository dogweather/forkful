---
title:                "Arduino: ディレクトリが存在するかを調べる"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認する必要がある理由は、プログラムの中で特定のファイルを動的に読み込む必要があるからです。例えば、センサーからのデータをログファイルに保存する場合、プログラムが実行されるたびに新しいファイルが作成されるため、事前にそのディレクトリの存在を確認する必要があります。

## 方法
ディレクトリが存在するかどうかを確認するには、```File::exists()```関数を使います。例えば以下のように書きます：

```Arduino
#include <SPI.h>
#include<SD.h>

File dataFile; // ファイルオブジェクトを作成

void setup() {
  Serial.begin(9600);
  
  if(!SD.begin(10)){  // SDカードが初期化できない場合
    Serial.println("SD card initialization failed!");
    return;  // エラーを出力してプログラムを実行しない
  }
  
  // dataフォルダを作成
  if(!SD.exists("/data")){
    SD.mkdir("/data");
    Serial.println("Created 'data' directory");
  }
  
  // dataフォルダが存在するかどうかを確認
  if(SD.exists("/data")){
    Serial.println("Directory 'data' exists!");
  } else {
    Serial.println("Directory 'data' does not exist!");
  }
}

void loop() {
  // メインのプログラムを実行
}

```
実行結果：
```
Created 'data' directory
Directory 'data' exists!
```

## ディープダイブ
さらに詳しく知りたい方のために、ディレクトリの存在を確認する方法について詳しく説明します。ディレクトリが存在するかどうかを確認するには、```File::exists()```関数を使用します。この関数の戻り値は```bool```型であり、ディレクトリが存在する場合は```true```を、存在しない場合は```false```を返します。

また、ディレクトリが存在しない場合は、```File::mkdir()```関数を使用してディレクトリを作成することもできます。この関数は新しいディレクトリを作成し、作成できた場合は```true```を、作成できなかった場合は```false```を返します。

## さらに参考になる情報
- [Official Arduino Documentation on File::exists()](https://www.arduino.cc/en/Reference/FileExists)
- [Official Arduino Documentation on File::mkdir()](https://www.arduino.cc/en/Reference/FileMkdir)
- [SD library for Arduino](https://www.arduino.cc/en/Reference/SD)