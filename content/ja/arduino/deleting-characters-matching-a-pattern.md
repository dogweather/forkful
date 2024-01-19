---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字パターンに一致する文字を削除する、これは具体的には特定のパターンに一致する全ての文字を対象の文字列から削除することを指します。これは、不要なホワイトスペースを削除したり、特定の記号を除去したりする際などによく使用されます。

## 実践方法：

以下に、`Arduino`のコード例を示します。文字列から特定の文字('a'と'b')を削除します。

```Arduino
String sentence = "This is a basic Arduino example.";
sentence.replace("a", ""); 
sentence.replace("b", ""); 
Serial.println(sentence);
```

出力：

```Arduino
"This is sic Arduio exmple."
```

これは、`replace()`関数を使用してパターンに一致する文字をブランク('')に置き換えることで、特定の文字を削除しています。

## さらに深く:

この特性は、コンピュータプログラミングの初期段階から存在しており、文字列操作の基本とも言える機能です。他の方法としては正規表現を使用したパターンマッチングがありますが、Arduinoでは直接サポートされていないため利用が難しいです。

実装については、`replace()`関数の内部ではループと文字列の比較が行われており、適切な文字列の挿入と削除が管理されています。そのため、大量のデータで使用すると処理速度が遅くなる可能性がある点には注意が必要です。

## 参考情報：

以下のリンクから関連するトピックについて学ぶことができます:
- Arduinoの文字列操作: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/
- 更に詳しい文字列操作テクニック: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/