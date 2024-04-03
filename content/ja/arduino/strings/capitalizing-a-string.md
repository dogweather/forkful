---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.358467-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.470577-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306F\u4E3B\u306B\u30CF\u30FC\u30C9\u30A6\u30A7\u30A2\u3068\u306E\
  \u3084\u308A\u53D6\u308A\u3067\u77E5\u3089\u308C\u3066\u3044\u307E\u3059\u304C\u3001\
  `String`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u901A\u3058\u3066\u57FA\u672C\
  \u7684\u306A\u6587\u5B57\u5217\u64CD\u4F5C\u6A5F\u80FD\u3082\u5099\u3048\u3066\u3044\
  \u307E\u3059\u3002\u3057\u304B\u3057\u3001\u4E0A\u4F4D\u30EC\u30D9\u30EB\u306E\u8A00\
  \u8A9E\u306B\u898B\u3089\u308C\u308B\u76F4\u63A5\u7684\u306A`capitalize`\u95A2\u6570\
  \u306F\u5B58\u5728\u3057\u307E\u305B\u3093\u3002\u3057\u305F\u304C\u3063\u3066\u3001\
  \u6587\u5B57\u5217\u3092\u53CD\u5FA9\u51E6\u7406\u3057\u3066\u30B1\u30FC\u30B9\u5909\
  \u63DB\u3092\u9069\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u5927\u6587\u5B57\u5316\
  \u3092\u5B9F\u88C5\u3057\u307E\u3059."
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
