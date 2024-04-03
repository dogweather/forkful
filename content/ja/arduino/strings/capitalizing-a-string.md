---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.358467-07:00
description: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u3059\
  \u308B\u64CD\u4F5C\u306F\u3001\u6587\u5B57\u5217\u306E\u5404\u5358\u8A9E\u306E\u6700\
  \u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3057\u3001\u6B8B\
  \u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u30C7\u30FC\u30BF\u306E\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3084\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\u898F\
  \u5316\u306B\u304A\u3044\u3066\u4E00\u822C\u7684\u3067\u3042\u308A\u3001\u4E00\u8CAB\
  \u6027\u3092\u4FDD\u3061\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\u305B\u308B\
  \u305F\u3081\u306B\u884C\u308F\u308C\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.470577-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u3059\
  \u308B\u64CD\u4F5C\u306F\u3001\u6587\u5B57\u5217\u306E\u5404\u5358\u8A9E\u306E\u6700\
  \u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3057\u3001\u6B8B\
  \u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\
  \u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u30C7\u30FC\u30BF\u306E\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3084\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\u898F\
  \u5316\u306B\u304A\u3044\u3066\u4E00\u822C\u7684\u3067\u3042\u308A\u3001\u4E00\u8CAB\
  \u6027\u3092\u4FDD\u3061\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\u305B\u308B\
  \u305F\u3081\u306B\u884C\u308F\u308C\u307E\u3059\u3002."
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
