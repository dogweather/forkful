---
date: 2024-01-26 03:36:55.769706-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\u524A\u9664\
  \u3059\u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\u3080\u30B7\u30F3\
  \u30B0\u30EB\uFF08`'`\uFF09\u307E\u305F\u306F\u30C0\u30D6\u30EB\uFF08`\"`\uFF09\u30AF\
  \u30A9\u30FC\u30C8\u6587\u5B57\u306E\u4EFB\u610F\u306E\u30A4\u30F3\u30B9\u30BF\u30F3\
  \u30B9\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u6B63\u898F\
  \u5316\u3059\u308B\u3001\u6587\u5B57\u5217\u3092\u6BD4\u8F03\u306E\u305F\u3081\u306B\
  \u6E96\u5099\u3059\u308B\u3001\u307E\u305F\u306F\u8AA4\u3063\u3066\u5F15\u7528\u7B26\
  \u3092\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3068\u3057\u3066\u542B\u3080\u30C6\u30AD\
  \u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u51E6\u7406\u3059\u308B\u305F\u3081\u306B\u3001\
  \u3057\u3070\u3057\u3070\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:16.028251-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\u524A\u9664\
  \u3059\u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\u3080\u30B7\u30F3\
  \u30B0\u30EB\uFF08`'`\uFF09\u307E\u305F\u306F\u30C0\u30D6\u30EB\uFF08`\"`\uFF09\u30AF\
  \u30A9\u30FC\u30C8\u6587\u5B57\u306E\u4EFB\u610F\u306E\u30A4\u30F3\u30B9\u30BF\u30F3\
  \u30B9\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u6B63\u898F\
  \u5316\u3059\u308B\u3001\u6587\u5B57\u5217\u3092\u6BD4\u8F03\u306E\u305F\u3081\u306B\
  \u6E96\u5099\u3059\u308B\u3001\u307E\u305F\u306F\u8AA4\u3063\u3066\u5F15\u7528\u7B26\
  \u3092\u6587\u5B57\u5217\u306E\u4E00\u90E8\u3068\u3057\u3066\u542B\u3080\u30C6\u30AD\
  \u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u51E6\u7406\u3059\u308B\u305F\u3081\u306B\u3001\
  \u3057\u3070\u3057\u3070\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## はじめに & なぜ？
文字列からクォートを削除するとは、テキストを囲むシングル（`'`）またはダブル（`"`）クォート文字の任意のインスタンスを取り除くことを意味します。プログラマーは、入力を正規化する、文字列を比較のために準備する、または誤って引用符を文字列の一部として含むテキストデータを処理するために、しばしばこれを行います。

## 方法：
Arduinoで文字列からクォートを削除するには、文字をループ処理してクォート文字なしで文字列を再構築できます。例えば：

```arduino
String removeQuotes(String str) {
  String result = ""; // 結果を保持するための空の文字列を作成
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // 各文字を確認
      result += str[i]; // クォートでなければ結果に追加
    }
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Hello, World!'";
  Serial.println(removeQuotes(testStr)); // Hello, World! と表示されるべき
}

void loop() {
  // ここでは何もしなくてよい
}
```

シリアルモニターでのサンプル出力は：
```
Hello, World!
```

## 詳細解説
文字列から文字を削除するという概念はArduinoに独特のものではなく、多くのプログラミング環境で一般的です。歴史的に、文字列操作機能は、開発者がデータをクリーンにし、効果的に解析することを可能にするために、プログラミング言語の中心的な部分を占めてきました。

上記のように手動でループ処理して新しい文字列を構築する方法に加えて、代替方法もあります。例えば、クォートを空の文字列に置き換えるために `replace()` メソッドを使用することができますが、可読性の管理やエスケープ文字の管理という点でトレードオフがあります。

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // すべてのダブルクォートを置き換える
  str.replace("\'", ""); // すべてのシングルクォートを置き換える
  return str;
}
```

トレードオフを理解することは重要です。ループメソッドは長い文字列に対しては遅くなる可能性がありますが、明示的でカスタマイズが容易です（例えば、先頭と末尾のクォートだけを削除する必要がある場合）。`replace()` メソッドはより簡潔で一般に早いですが、文字列内のエスケープされたクォート文字を処理する必要がある場合は、扱いが難しくなります。

## 関連情報
- Arduino文字列参照：https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- W3SchoolsのC++の文字列操作ガイド（Arduinoの言語に関連）：https://www.w3schools.com/cpp/cpp_strings.asp
- C++（Arduinoの基本言語）における文字列操作についてのStack Overflowの議論：https://stackoverflow.com/questions/tagged/string+cpp
