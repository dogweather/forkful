---
date: 2024-01-20 17:45:54.352333-07:00
description: "How to: Java\u3067\u306F `substring` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\
  \u3063\u3066\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u7C21\u5358\u306B\u53D6\
  \u308A\u51FA\u305B\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.930382-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u306F `substring` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3063\u3066\
  \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u7C21\u5358\u306B\u53D6\u308A\u51FA\
  \u305B\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
