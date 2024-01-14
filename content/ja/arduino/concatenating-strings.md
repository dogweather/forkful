---
title:                "Arduino: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

コンピューターに文字列を結合することで、新しい文字列を作成することができます。これはArduinoプログラミングでとても便利です。例えば、センサーやディスプレイから得られた情報を一つの文章にまとめることができます。

## 使い方

結合する文字列を用意し、それぞれを「+」記号でつなげることで、新しい文字列を作成することができます。以下は例です。

```Arduino
String name = "太郎";
String message = "こんにちは、私の名前は" + name + "です。";
Serial.println(message);
```

出力は以下のようになります。

```
こんにちは、私の名前は太郎です。
```

## 深堀り

Arduinoのプログラミング言語では、文字列を扱う際にString型を使用します。String型は、複数の文字を格納し、結合することもできるデータ型です。また、文字列を結合する方法として、「+=」演算子を使用することもできます。

また、ここで注意すべき点として、文字列を頻繁に結合するとメモリの使用量が増えてしまうため、プログラムを効率的に作成するためには、文字列の結合をなるべく避けるようにすることが重要です。

## 参考リンク

- [Arduino Stringクラスリファレンス](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino Stringsチュートリアル](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)