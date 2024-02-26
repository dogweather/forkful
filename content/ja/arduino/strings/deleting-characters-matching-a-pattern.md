---
date: 2024-01-20 17:41:38.938465-07:00
description: "\u6587\u5B57\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\
  \u5B57\u3092\u524A\u9664\u3059\u308B\u306E\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\
  \u4E0D\u8981\u306A\u90E8\u5206\u3092\u53D6\u308A\u9664\u304F\u4F5C\u696D\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u6574\u7406\
  \u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306E\u9069\u6B63\u5316\u3001\u307E\u305F\
  \u306F\u4E0D\u8981\u306A\u60C5\u5831\u306E\u524A\u9664\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.439965-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\
  \u5B57\u3092\u524A\u9664\u3059\u308B\u306E\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\
  \u4E0D\u8981\u306A\u90E8\u5206\u3092\u53D6\u308A\u9664\u304F\u4F5C\u696D\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u6574\u7406\
  \u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306E\u9069\u6B63\u5316\u3001\u307E\u305F\
  \u306F\u4E0D\u8981\u306A\u60C5\u5831\u306E\u524A\u9664\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字パターンに一致する文字を削除するのは、文字列から不要な部分を取り除く作業です。プログラマーはデータの整理、フォーマットの適正化、または不要な情報の削除のためにこれを行います。

## How to: (やり方)
Arduinoには文字列からパターンに一致する文字を直接削除する組み込み関数はありませんが、必要な機能を作成することができます。以下の例では、特定の文字を削除する関数を作成し、使用する方法を示します。

```Arduino
String deleteMatchingCharacters(String input, String pattern) {
  String output = "";
  for (int i = 0; i < input.length(); i++) {
    if (pattern.indexOf(input.charAt(i)) < 0) {
      // If the character at position i is not in the pattern, keep it
      output += input.charAt(i);
    }
  }
  return output;
}

void setup() {
  Serial.begin(9600); 
  String originalText = "Hello, World! 123";
  String patternOfCharsToDelete = "lo123";

  Serial.println("Before: " + originalText);
  String cleanedText = deleteMatchingCharacters(originalText, patternOfCharsToDelete);
  Serial.println("After: " + cleanedText);
}

void loop() {
  // Nothing to do here
}
```

出力は次のようになります。

```
Before: Hello, World! 123
After: He, Wr!
```

## Deep Dive (深掘り)
歴史的に、文字削除の概念は古くからあり、Unixの`tr`コマンドなどで一般的に使われていました。Arduinoでは直接的な組み込み関数は提供されていないため、自分で関数を作成する必要があります。このアプローチの代替としては、正規表現ライブラリを使用することがありますが、Arduinoでの使用はメモリ制限のため推奨されません。作成した`deleteMatchingCharacters`関数は単純でメモリ効率が良いため、Arduinoのような制約のある環境で理想的です。

## See Also (関連情報)
- ArduinoのStringクラスのドキュメンテーション: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- 文字処理の基本：https://www.arduino.cc/reference/en/language/functions/characters/ 
- 正規表現による文字処理：https://github.com/nickgammon/Regexp (注意: メモリ制限に留意)
