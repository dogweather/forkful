---
title:                "パターンに一致する文字を削除する"
aliases:
- /ja/arduino/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:38.938465-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
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
