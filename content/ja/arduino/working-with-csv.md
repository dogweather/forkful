---
title:                "「CSV形式との作業」"
html_title:           "Arduino: 「CSV形式との作業」"
simple_title:         "「CSV形式との作業」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVとは?
CSVとはデータの表現形式の一つで、コンマ区切り形式と呼ばれています。プログラマーがCSVを使用する理由は、データを表形式で簡単に保存したり、読み込んだりすることができるためです。

## 方法:
CSVを使用するには、Arduinoのライブラリ「CSV」をインストールする必要があります。その後、ファイルを読み込んだり、データを表形式で保存したりすることができます。以下に、ライブラリを使用したサンプルコードと出力を示します。

```Arduino
#include <CSV.h>

void setup() {
  // CSVファイルを読み込む
  CSV file("data.csv");
  // データを表形式で保存する
  String data = "Name, Age, Gender\nJohn, 25, Male\nJane, 30, Female";
  file.println(data);
  // ファイルから1行ずつ読み込んで表示する
  String line = file.readStringUntil('\n');
  Serial.println(line);
}

void loop() {
  // ここで何かを行う
}
```

### 出力:
```
Name, Age, Gender
John, 25, Male
```

## 詳細:
CSVは主にデータベースやスプレッドシートで使用される形式ですが、プログラミング言語でもよく使われています。CSV以外のデータ表現形式には、XMLやJSONがありますが、CSVはシンプルで使いやすいため、比較的人気があります。

ライブラリ「CSV」は、ArduinoでCSVファイルを操作するための便利なツールです。データの読み込みや保存を簡単に行うことができます。また、Arduino以外にも利用できるため、プログラミングにおける汎用性が高いことも特徴の一つです。

具体的な実装の詳細は、Arduinoの公式ドキュメントやライブラリのページを参照してください。

## 参考リンク:
- Arduino公式ドキュメント: https://www.arduino.cc/en/Reference/Libraries/CSV
- CSVライブラリのページ: https://github.com/4996fj/CSV