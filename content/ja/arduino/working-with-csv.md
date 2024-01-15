---
title:                "CSV での作業"
html_title:           "Arduino: CSV での作業"
simple_title:         "CSV での作業"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使うのか?

CSV（Comma Separated Values）は、データをテキスト形式で表現するためのフォーマットの一種です。ArduinoでCSVを扱うことで、データの収集や処理を簡単に行うことができます。例えば、温度や湿度などのセンサーデータをCSV形式で収集し、コンピューター上で簡単に分析・グラフ化することができます。CSVを使うことで、データの管理・可視化がしやすくなります。

## 使い方

まずは、Arduinoのライブラリから`CSV.h`をダウンロードし、プログラムにインクルードします。

```Arduino
#include <CSV.h>
```

次に、CSVファイルを扱うためのオブジェクトを作成します。

```Arduino
CSV myCSV;
```

CSVファイルを新しく作成する場合は、`create()`関数を使います。必要なデータの数だけ、`write()`関数を使って値を追加していきます。

```Arduino
myCSV.create("data.csv"); // CSVファイルを作成

myCSV.write("temperature", 25); // "temperature"というヘッダーと25という値を追加
myCSV.write("humidity", 50); // "humidity"というヘッダーと50という値を追加
```

既存のCSVファイルを読み込む場合は、`open()`関数を使います。`read()`関数を使うことで、1行ずつ値を取得することができます。

```Arduino
myCSV.open("data.csv"); // 既存のCSVファイルを読み込む

int temp = myCSV.read("temperature"); // "temperature"というヘッダーの値を取得し、変数に格納する
int humid = myCSV.read("humidity"); // "humidity"というヘッダーの値を取得し、変数に格納する
```

## 深堀り

CSVファイルは、コンマ`,`や改行文字`\n`で区切られたテキストファイルです。Arduinoでは、`Serial.print()`を使って各値をコンマで区切ることで、簡単にCSV形式のデータを作ることができます。また、`SD.h`ライブラリを使えば、CSVファイルをSDカードに保存することもできます。

さらに、`parse()`関数を使うことで、CSVファイルから特定のヘッダーの値を取得することも可能です。また、`remove()`関数を使うことで、特定のヘッダーを持つ値を削除することもできます。

## 関連リンク

- [CSV.hのダウンロード](https://github.com/DzikuVx/CSV)
- [SD.hライブラリの使い方](https://www.arduino.cc/en/Reference/SD)
- [parse()関数の使い方](https://www.arduino.cc/en/Reference/SDParse)
- [remove()関数の使い方](https://www.arduino.cc/en/Reference/SDRemove)