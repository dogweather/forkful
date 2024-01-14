---
title:                "Arduino: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを作成するのには、さまざまな目的があります。例えば、センサーから収集したデータを保存するため、またはプロジェクトの進捗を記録するために使われることがあります。Arduinoを使ってテキストファイルを作成することによって、データや情報を簡単に保存し、後で見返すことができます。

## 作り方
テキストファイルを作成するには、Serialモニターを利用します。まず、```Serial.begin(9600)```を使用してシリアル通信を開始し、次にファイルを作成するための準備をします。例えば、```File dataFile = SD.open("data.txt", FILE_WRITE)```のようにコードを書きます。そして、```dataFile.println()```を使用してデータをファイルに書き込みます。ファイルを閉じるには、```dataFile.close()```を使用します。下記のコードは、温度データを取得し、テキストファイルに書き込む例です。

```
void setup()
{
  Serial.begin(9600);
  if(!SD.begin())
  {
    Serial.println("SDカードの初期化に失敗しました。");
    return;
  }
  Serial.println("SDカードの初期化に成功しました。");
}

void loop()
{
  float temperature = analogRead(A0) * 0.488;
  File dataFile = SD.open("temperatures.txt", FILE_WRITE);
  dataFile.println(temperature);
  dataFile.close();
  delay(1000);
}
```

上記のコードを実行すると、SDカードに```temperatures.txt```という名前のテキストファイルが作成され、温度データが1秒ごとに書き込まれます。ファイルを開いてみると、データが正しく保存されていることが確認できます。

## ディープダイブ
テキストファイルを作成するには、SDライブラリを使用します。SDライブラリには、SDカードへのアクセスを支援するさまざまな機能が備わっています。テキストファイルを作成する前に、まずSDカードを初期化する必要があります。```SD.begin()```を使用して初期化し、成功するとSDカードの使用準備が完了します。また、SDカードにデータを書き込む前に、ファイルが既に存在するかどうかを確認することも重要です。既にファイルが存在する場合は、ファイルが上書きされてしまう可能性があります。そのため、```SD.exists("data.txt")```を使用してファイルが既に存在する場合は、新しく作成するファイルの名前を変更するようにコードを書くことができます。

## はてな
もしもArduinoでテキストファイルを作成する際にエラーが発生した場合は、SDカードが正しく接続されているかどうかを確認することが重要です。また、```Serial.println()```を使用して、エラーメッセージをシリアルモニターに表示することで、どこでエラーが発生しているかを確認することもできます。さらに、テキストファイル以外にも、CSVファイルやJSONファイルなどさまざまな形式でデータを保存すること