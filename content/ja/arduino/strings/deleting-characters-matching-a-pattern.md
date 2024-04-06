---
date: 2024-01-20 17:41:38.938465-07:00
description: "How to: (\u3084\u308A\u65B9) Arduino\u306B\u306F\u6587\u5B57\u5217\u304B\
  \u3089\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u76F4\
  \u63A5\u524A\u9664\u3059\u308B\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u306F\u3042\u308A\
  \u307E\u305B\u3093\u304C\u3001\u5FC5\u8981\u306A\u6A5F\u80FD\u3092\u4F5C\u6210\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u3067\
  \u306F\u3001\u7279\u5B9A\u306E\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u95A2\u6570\
  \u3092\u4F5C\u6210\u3057\u3001\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.293284-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Arduino\u306B\u306F\u6587\u5B57\u5217\u304B\u3089\u30D1\
  \u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u76F4\u63A5\u524A\
  \u9664\u3059\u308B\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u306F\u3042\u308A\u307E\u305B\
  \u3093\u304C\u3001\u5FC5\u8981\u306A\u6A5F\u80FD\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u3067\u306F\u3001\
  \u7279\u5B9A\u306E\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u95A2\u6570\u3092\u4F5C\
  \u6210\u3057\u3001\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\
  \u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

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
