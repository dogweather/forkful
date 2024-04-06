---
date: 2024-01-20 17:47:43.107852-07:00
description: "How to: Java\u3067\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\
  \u308B\u306B\u306F\u3001`.length()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3044\u307E\
  \u3059\u3002\u30B7\u30F3\u30D7\u30EB\u306A\u306E\u3067\u3001\u3059\u3050\u306B\u30DE\
  \u30B9\u30BF\u30FC\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.831265-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B\u306B\
  \u306F\u3001`.length()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3044\u307E\u3059\u3002\
  \u30B7\u30F3\u30D7\u30EB\u306A\u306E\u3067\u3001\u3059\u3050\u306B\u30DE\u30B9\u30BF\
  \u30FC\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

## How to:
Javaで文字列の長さを求めるには、`.length()`メソッドを使います。シンプルなので、すぐにマスターできます。

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "こんにちは";
        int length = greeting.length();
        System.out.println("文字列の長さ: " + length);
    }
}
```

このコードを実行すると、次の出力が得られます。
```
文字列の長さ: 5
```

## Deep Dive
文字列の長さを見つける機能は、Javaの初期バージョンから存在しています。`.length()`メソッドは `String`クラス内にあります。`String` オブジェクトが不変（イミュータブル）であるため、一度作成された文字列の長さは変わりません。結果的に `.length()`メソッドは、計算済みの長さを返します。

実は、文字列の長さを見つけるための別の方法もあります。たとえば、Java 8では `CharSequence` インターフェースの `chars().count()` を使うこともできます。

```java
long length = greeting.chars().count();
```

しかし、だいたいの場合 `.length()` がシンプルで直感的です。`chars().count()` はストリームを生成して処理するため、単純な文字列の長さを知りたいときにはオーバーヘッドになります。

## See Also
- [`String` documentation in Oracle's Java docs](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Java String tutorial from W3Schools](https://www.w3schools.com/java/java_strings.asp)
- [Java 8 `Stream` documentation](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html)
