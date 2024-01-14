---
title:    "Arduino: The title of an article on computer programming: 文字列の大文字化"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

あなたはArduinoプログラムを書くときに、文字列を大文字化する必要があるかもしれません。例えば、センサーから受け取ったデータを処理するときや、ディスプレイに表示するときには、大文字化した方が読みやすい場合があります。

## 方法

文字列を大文字化するためには、Arduinoの`toUpperCase()`関数を使用します。この関数は、指定した文字列をすべて大文字にすることができます。例えば、以下のようにコードを書くことができます。

```Arduino
String message = "Hello world!";
message.toUpperCase();
Serial.println(message);
```

このコードを実行すると、シリアルモニターには`HELLO WORLD!`という文字列が表示されます。

## 深堀り

Arduinoの`toUpperCase()`関数は、文字列の先頭以外のすべての文字を大文字に変換します。つまり、英数字や記号などの文字以外のものは変換されません。また、この関数は元の文字列を変更するのではなく、大文字化した新しい文字列を返します。そのため、元の文字列の内容が変わることはありません。

## その他の情報

上記の例では、文字列を変数に格納してから`toUpperCase()`関数を使用しましたが、直接関数を使用することもできます。例えば、以下のように書くことができます。

```Arduino
Serial.println("hello world!".toUpperCase());
```

この場合も、シリアルモニターには`HELLO WORLD!`が表示されます。

## おわりに

文字列を大文字化する方法を紹介しましたが、もちろん大文字化の必要がない場合は関数を使用する必要はありません。適宜使い分けるようにしましょう。

## 関連リンク

- [Arduinoの公式ドキュメント](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/touppercase/)
- [文字列の操作についての日本語の記事](https://garretlab.web.fc2.com/arduino/lab/string.html)