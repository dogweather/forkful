---
title:                "Arduino: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングでテキストの検索と置換をする理由は、効率的なコードの作成や繰り返し作業の省略のためです。

## 方法
検索と置換を行うためのコード例を以下に示します。その後、コードの出力結果も表示されます。   
```Arduino
void setup() {      
  Serial.begin(9600);   
  String text = "今日はいい天気です。";   
  // "今日"を"明日"に置換する   
  text.replace("今日", "明日");   
  Serial.println(text);   
}   
```   
出力： "明日はいい天気です。"

## 掘り下げる
検索と置換は、プログラミングにおいて非常に重要な機能です。文字列操作やデータの編集に欠かせないものであり、コードをより洗練されたものにすることができます。
しかし、検索と置換を使用する際には注意が必要です。意図しない処理が行われないように、コードをしっかりとテストすることが重要です。

## 参考リンク
- [String replace() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)   
- [String manipulation - Arduino Tutorial](https://www.arduino.cc/en/Tutorial/StringManipulation)