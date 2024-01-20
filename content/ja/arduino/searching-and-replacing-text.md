---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストの検索と置換は、特定の文字列を見つけて書き換えるというプログラミングの一部です。これにより、情報を効 efficiently、効果的に更新し、再利用することができます。

## 実践

```Arduino
String s = "Hello, world!";
s.replace("world", "Arduino");
Serial.println(s); // "Hello, Arduino!"が出力されます
```

上記は、"world"を"Arduino"に置き換える簡単な例です。

## ディープダイブ

検索と置換の起源は、古くはワードプロセッサやテキストエディタに remontで、プログラマーやライターがテキストの修正を効率化できるようになりました。Arduinoでは、「replace」メソッドを使用してこれを実現しています。

代替手段としては、「substring」や「toCharArray」などを利用した自己実装が考えられますが、組み込みメソッド使用の方が効率的です。

実装上の詳細としては、"replace"メソッドは巡回検索を使用して対象文字列を探し、見つかった位置の文字列を新しい文字列で置換します。

## 関連情報

以下リンクでより深く学べます：
- [Arduino String Reference（英語）](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
以上で、テキストの検索と置換については一通り理解できたはずです。これであなたもArduinoにおけるテキスト処理の達人に一歩近づいたはず！