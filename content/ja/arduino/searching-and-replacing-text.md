---
title:                "テキストの検索と置換"
html_title:           "Arduino: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

文字列の検索と置換をする理由は様々です。例えば、コンピュータや電子機器のプログラムを作る際に、特定の文字列を一括で変更する必要がある場合があります。また、大量の文書を編集する際にも、検索して置換することで時間を節約することができます。

## 方法

文字列の検索と置換をするために、Arduinoのプログラムを作成する方法を紹介します。まずは、"```Arduino ...```"のコードブロック内に、検索したい文字列と置換したい文字列を指定します。次に、"```while (Serial.available())```"のループを使って、読み込んだ文字列の中から検索文字列を探し、置換文字列に置き換えます。最後に、"```Serial.println()```"を使って置換された文字列を出力すれば、検索と置換が成功します！

```
Arduinoの例：

String search_text = "こんにちは";
String replace_text = "こんばんは";

while (Serial.available()) {
  String input = Serial.readStringUntil('\n');
  
  if (input.indexOf(search_text) != -1) { 
    input.replace(search_text, replace_text); 
  }
  
  Serial.println(input);
}
```

この例では、"こんにちは"という文字列を"こんばんは"に置換するプログラムが作られています。

## 深堀り

文字列の検索と置換は、ArduinoのStringクラス内の関数を使って実現されます。"indexOf()"関数は、指定した文字列が見つかった場合にその位置を返し、見つからない場合には-1を返します。また、"replace()"関数は、指定した文字列を別の文字列に置き換えます。これらの関数を組み合わせることで、文字列の検索と置換を簡単に行うことができます。

## 参考リンク

- [ArduinoのStringクラス公式ドキュメント](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Stringクラスの関数一覧](https://www.electronics-lab.com/projects/arduino-string-functions/)
- [Arduinoを使った文字列の検索と置換の方法](https://www.instructables.com/id/How-to-Search-Replace-Strings-on-Arduino/#:~:text=To%20replace%20multiple%20occurrences%20of%20a%20single%20character%20in%20a%20string%2C%20we%20can%20use%20the%20replace()%20function%20along%20with%20the%20indexOf()%20function%20in%20a%20while%20loop.)