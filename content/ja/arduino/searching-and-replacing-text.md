---
title:    "Arduino: テキストの検索と置換"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちはテキストの検索と置換に取り組むのでしょうか？テキストの検索と置換は、Arduinoプログラミングにおいて非常に重要な機能です。テキストを検索して置換することで、データを簡単に変更したり、特定の文字列を見つけて処理したりすることができます。この機能はArduinoプログラムをより柔軟に、そして強力にするのに役立ちます。

## 方法

ここでは、テキストの検索と置換を行うための基本的なコード例を紹介します。まず、テキストを検索して特定の文字列を置換する例を見てみましょう。

```Arduino
String text = "Hello World";
text.replace("World", "Arduino");

Serial.println(text);
```
上記の例では、変数`text`に保存されている文字列`Hello World`の中の`World`という文字列が`Arduino`に置換されます。コードを実行すると、`Hello Arduino`という文字列がシリアルモニターに表示されます。

次に、文字列の中から特定の文字を検索してその文字を置換する例を見てみましょう。

```Arduino
String text = "I love Arduino";
text.replace("love", "use");

Serial.println(text);
```
上記のコードでは、変数`text`の中から`love`という文字列を検索して、その文字列を`use`に置換します。実行すると、`I use Arduino`という文言が表示されます。

## 詳細を掘り下げる

テキストの検索と置換には様々な方法があります。例えば、正規表現を使うことでより複雑な検索や置換が可能になります。また、マルチバイト文字を扱う場合は、`replace()`ではなく`replaceAll()`を使用する必要があります。さらに、Arduinoの不思議な文字コードの扱いも影響してくることがあります。そのため、テキストの検索と置換を行う際には、十分に文書を読み込み、詳細を理解することが重要です。

## 参考リンク

- [Arduino Reference - replace()](https://www.arduino.cc/reference/en/language/functions/strings/stringfunctions/replace/)
- [Arduino Reference - replaceAll()](https://www.arduino.cc/reference/en/language/functions/strings/stringfunctions/replaceall/)
- [正規表現を使ったテキストの検索と置換](https://www.geosite.info/programming/regexp/search-replace.html)
- [マルチバイト文字を扱う際の注意点](https://stackoverflow.com/questions/5400197/arduino-how-to-replace-multiple-bytes-string-in-a-large-single-bytes-string)
- [Arduinoでの日本語文字コードの扱いについて](https://futurismo.biz/archives/3044)