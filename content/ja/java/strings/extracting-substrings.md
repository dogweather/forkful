---
aliases:
- /ja/java/extracting-substrings/
date: 2024-01-20 17:45:54.352333-07:00
description: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3068\u306F\u6587\
  \u5B57\u5217\u306E\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\
  \u3002\u3053\u306E\u64CD\u4F5C\u306F\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3084\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u5909\u66F4\u6642\u306B\u3088\u304F\u4F7F\u308F\u308C\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.791472
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3068\u306F\u6587\
  \u5B57\u5217\u306E\u4E00\u90E8\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\
  \u3002\u3053\u306E\u64CD\u4F5C\u306F\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3084\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u5909\u66F4\u6642\u306B\u3088\u304F\u4F7F\u308F\u308C\
  \u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why?
サブストリング抽出とは文字列の一部を取り出すことです。この操作はデータの解析やフォーマット変更時によく使われます。

## How to:
Javaでは `substring` メソッドを使ってサブストリングを簡単に取り出せます。例を見てみましょう。

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "こんにちは、世界！";
        String greeting = fullString.substring(0, 5); // "こんにちは" を取り出す
        
        System.out.println(greeting); // "こんにちは" を表示
    }
}
```

出力:
```
こんにちは
```

## Deep Dive
Javaにおける `substring` メソッドはJava 1から存在しています。Java 1.4までは文字列を実際にコピーしていましたが、メモリ使用量の問題から、Java 7では新しい文字列を作る方式に変わりました。`substring` の代わりに `String` クラスの `split`, `charAt`, そして `pattern` クラスを使うことも可能ですが、複雑な操作になる場合は、`substring` が便利です。

## See Also
- OracleのJavaドキュメント: [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- 文字列操作に関する詳細なガイド: [Baeldung - Guide to String](https://www.baeldung.com/java-string)
