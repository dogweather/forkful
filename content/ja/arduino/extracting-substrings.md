---
title:                "「サブストリングの抽出」"
html_title:           "Arduino: 「サブストリングの抽出」"
simple_title:         "「サブストリングの抽出」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## どうして

抽出された部分文字列を取得することは、コーディングの際により柔軟性をもたらし、データ処理をより簡単にする方法です。例えば、センサーから受け取った値の中から特定の情報を抽出したい場合、部分文字列を取得することで必要な情報を簡単に抽出することができます。

## 使い方

部分文字列を取得するには、```Arduino.substring()``` 関数を使用します。この関数は、2つの引数を必要とします。最初の引数は、取得したい部分文字列の開始位置を示すインデックス番号で、2番目の引数は終了位置を示すインデックス番号です。

```Arduino
String myString = "Hello world!";
String subString = myString.substring(6, 11);
Serial.println(subString); // 出力: world 
```

この例では、"Hello world!"という文字列の、6番目から11番目の文字を抽出し、"world"という部分文字列を取得しています。

別の例として、センサーから受け取った文字列の中から数値だけを抽出し、その値を使用してLEDを制御することができます。

```Arduino
String sensorString = "ID:453,Value:987";
int value = sensorString.substring(14).toInt(); // 文字列から数値だけを抽出し、数値に変換
analogWrite(ledPin, value); // 抽出した値を使用してLEDを制御
```

このように、部分文字列を取得することで、必要な情報を簡単に抽出し、利用することができます。

## 詳細を見る

さらに深く部分文字列を理解するには、以下のリンクを参考にしてください。

- [Arduino Reference - String.substring()](https://www.arduino.cc/reference/en/language/variables/data-types/substring/)
- [Tech Target - Substring](https://searchsqlserver.techtarget.com/definition/substring)
- [Stack Overflow - How to extract a substring from a string?](https://stackoverflow.com/questions/1871230/how-to-extract-a-substring-from-a-string)

## 詳しくは

抽出された部分文字列は、より簡潔にコードを書くことができるだけでなく、データ処理をより簡単にすることができます。是非、部分文字列を積極的に活用してみてください。

## 参考

- [Markdown記法 チートシート - Qiita](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)
- [Markdown マークダウンチートシート - bungu](https://bun.gy/markdown-cheat-sheet)