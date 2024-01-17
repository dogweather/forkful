---
title:                "サブストリングの抽出"
html_title:           "Arduino: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Arduinoで部分文字列を抽出する方法

## なに？なんで？

部分文字列の抽出とは、文字列の中から特定の部分を取り出すことです。プログラマーはこの技術を使用して、必要な情報をより簡単に取得することができます。

## 方法

```arduino
// 文字列の定義
String sentence = "こんにちは世界！";

// 部分文字列を抽出する
String substring = sentence.substring(3,7);

// 出力を表示する
Serial.println(substring); // 出力: ちは世界
```

## 詳しく見る

部分文字列の抽出は、文字列処理において非常に重要な概念です。これは、プログラマーが特定の情報を正確に抽出することができるようにするために使用されます。部分文字列を抽出することにより、文字列の中から必要な情報を取得することが容易になります。

部分文字列の抽出には、いくつかの代替手段があります。例えば、文字列を配列に分割し、必要な部分だけを使用する方法もあります。しかし、この方法はより複雑であり、部分文字列の抽出よりも処理が遅くなる可能性があります。

部分文字列の抽出の実装には、主に2つの手法があります。一つは、文字列をある一定の位置から切り出すことで、もう一つはある特定の文字を探して切り出すことです。それぞれの実装には長所と短所があり、プログラマーは各状況に合わせて最適な手法を選ぶことが重要です。

## 関連リンク

- [String クラスリファレンス](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/)
- [正規表現と文字列操作](https://www.arduino.cc/reference/ja/language/functions/data-types/string/functions/substring/)
- [C++言語 - 文字列の部分文字列を抽出する方法](http://www.codingdict.com/article/2179)