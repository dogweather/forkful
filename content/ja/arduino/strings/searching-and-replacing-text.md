---
title:                "テキストの検索と置換"
aliases:
- /ja/arduino/searching-and-replacing-text.md
date:                  2024-01-20T17:56:56.962796-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

テキスト検索と置換とは、文字列の中から特定のパターンを見つけ出し、その部分を別のテキストで置き換えることです。プログラマーは、コードの修正、データの整理、または自動化処理のためにこれを行います。

## How to: (やり方)

```Arduino
String text = "Hello, world!";
String searchText = "world";
String replaceText = "Arduino";

text.replace(searchText, replaceText);
Serial.println(text); // "Hello, Arduino!"
```

## Deep Dive (深堀り)

テキストの検索と置換は文字列操作の基本で、1970年代の初期のテキストエディターから存在します。Arduinoにおける`String.replace()`メソッドはシンプルだが、大量のデータや長い文字列ではメモリ使用が問題になることがある。代替案としては、`char`配列を使う独自の関数を作ることで、サイズが大きい置換作業に対処することが可能です。 

## See Also (関連情報)

- Arduino String reference: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Memory Management with Arduino: [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
