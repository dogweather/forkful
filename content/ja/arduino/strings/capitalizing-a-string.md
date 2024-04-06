---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.358467-07:00
description: null
lastmod: '2024-04-05T21:53:43.291885-06:00'
model: gpt-4-0125-preview
summary: "\u3053\u3053\u306B\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\
  \u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u305B\u305A\u306B\u57FA\u672C\
  \u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
Arduinoは主にハードウェアとのやり取りで知られていますが、`String`オブジェクトを通じて基本的な文字列操作機能も備えています。しかし、上位レベルの言語に見られる直接的な`capitalize`関数は存在しません。したがって、文字列を反復処理してケース変換を適用することで、大文字化を実装します。

ここに、サードパーティーのライブラリを使用せずに基本的な例を示します：

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // 入力が空の場合は空の文字列を返す
  }
  input.toLowerCase(); // 最初に文字列全体を小文字に変換
  input.setCharAt(0, input.charAt(0) - 32); // 最初の文字を大文字に
  
  // スペースの後に続く文字を大文字に
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // 出力: "Hello Arduino World"
}

void loop() {
  // 空のループ
}
```

このコードスニペットは、`capitalizeString`関数を定義しており、最初に文字列全体を小文字に変換してケースを標準化します。次に、最初の文字とスペースの後に続く任意の文字を大文字にし、入力文字列の各単語を効果的に大文字化します。この基本的な実装はASCII文字コードを前提としており、完全なUnicodeサポートには調整が必要とされることに注意してください。

現在、Arduinoのエコシステムで文字列操作に特化した広く採用されたサードパーティーのライブラリはほとんどありません。主に、ハードウェアとのやり取りと効率に焦点を当てているためです。しかし、提供された例は、Arduinoのプログラミング環境内で文字列の大文字化を実現するための直接的な方法です。
