---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Arduino: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Arduinoでディレクトリが存在するか確認する方法

## 何となぜ？

ディレクトリの存在確認は、指定したディレクトリが存在するかどうかをチェックするプログラマの技法です。これは、必要なデータを保存するための場所が存在しているか、あるいはあるディレクトリに書き込み権限があるかどうかを知るために行われます。

## 方法：

以下にArduinoコードの例示とサンプル出力を示します。

```Arduino
#include <SD.h>

void setup(){
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SDカードの初期化に失敗しました");
    return;
  }
  if (SD.exists("/example")) {
    Serial.println("ディレクトリが存在します");
  } else {
    Serial.println("ディレクトリが存在しません");
  }
}

void loop(){}
```

出力（例）：

```
ディレクトリが存在します
```

## ディープダイブ：

歴史的な文脈では、Arduinoは組み込みシステムの開発を初心者に向けて簡単にすることを目指して生まれました。そしてSDカードライブラリが導入され、これによりディレクトリの存在を確認することが可能となりました。

代替手段として、次のような方法があります：「SD.open()」関数を使ってディレクトリ自体を開くことで、開くことができればディレクトリは存在するという意味になります。

実装の詳細については、「SD.exists()」関数がSDカードからディレクトリ名を探し、見つかれば「true」を返し、見つからなければ「false」を返すというプロセスが含まれます。

## 参照：

- [Arduino公式ドキュメンテーション](https://www.arduino.cc/en/SdCard/exists)
- [Arduino SDライブラリについて詳細に説明](https://www.arduino.cc/en/Reference/SD)
- [さらに進んだテクニックのためのArduinoプログラミングガイド](http://arduino-tutorials.net/tutorial/advanced-arduino-programming)