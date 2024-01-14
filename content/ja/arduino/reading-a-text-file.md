---
title:    "Arduino: テキストファイルの読み込み"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# なぜ
今日は、あなたがおそらくすでに知っているように、あなたのArduinoデバイスにテキストファイルを読み込む方法を学びましょう。デバイスとの通信やデータ管理において、テキストファイルは非常に便利で効率的な方法です。つまり、あなたのプログラムがサポートする最も基本的なファイルタイプです。

# 作り方
まず、テキストファイルを読み込むためには、ファイルをArduinoデバイスにアップロードする必要があります。そのためには、<File→Upload File>ツールを使用することができます。 

```
ArduinoのCREATEプラットフォーム。ファイル：Welcome.txt
をデバイスにアップロードします。
```

次に、ファイルを開くコードを書く必要があります。以下の例では、ファイル名を「Welcome.txt」とし、ファイルを読み込むためのシリアルポートを指定しています。

```
ArduinoのCREATEプラットフォーム。コード：
// テキストファイルの読み込み
File textFile = SD.open("Welcome.txt");

// ファイルが開かれたかどうかを確認
if (textFile) {
  // ファイルから１文字ずつ読み込み、シリアルモニタに出力
  while (textFile.available()) {
    Serial.write(textFile.read());
  }
  // ファイルを閉じる
  textFile.close();
} else {
  // ファイルが開けない場合はエラーメッセージを出力
  Serial.println("Error opening file");
}
```

出力例：

```
ArduinoのCREATEプラットフォーム。シリアルモニタ出力：

Welcome to Arduino!
This is a text file.
We hope you enjoy learning about reading text files on your device.
```

# 深堀り
テキストファイルを読み込む際、Arduinoはファイルをバイト（8ビット）単位で読み込みます。つまり、ファイル内の全ての文字やスペースは、それぞれ８ビットの数値に変換されます。具体的には、ASCIIコードと呼ばれる標準的な文字コードが使用されます。それぞれの文字に対応する数値は、[この表](https://www.ibm.com/docs/ja/i/7.4?topic=s-pcascii-table)で確認することができます。

また、テキストファイルを読み込む際には、ファイル内の改行やタブなどの特殊文字も読み込まれます。これらの特殊文字についても、同じように８ビットの数値に変換されます。

さらに、テキストファイルを読み込んだ後、Arduinoでそのデータを処理する場合、文字列として扱うことができます。このように、テキストファイルはデバイス間のデータ通信やデータ管理において非常に便利な役割を果たすことができます。

# その他参考リンク
- [ArduinoのSDライブラリドキュメント](https://www.arduino.cc/en/Reference/SD)
- [ArduinoのASCIIコード表](https://www.ibm.com/docs/ja/i/7.4?topic=s-pcascii-table)
- [ArduinoのStringライブラリドキュメント](https://www.arduino.cc/en/Reference/String