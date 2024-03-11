---
date: 2024-01-20 17:47:43.107852-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u306E\u63A2\u3057\u65B9\u3068\u306F\
  \uFF1F \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u6587\u5B57\u5217\u306E\u9577\
  \u3055\u304C\u5206\u304B\u308B\u3053\u3068\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\
  \u3001\u30EB\u30FC\u30D7\u306E\u5236\u5FA1\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u51E6\
  \u7406\u306A\u3069\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u5177\u4F53\u7684\u306A\
  \u9577\u3055\u3092\u77E5\u308B\u3053\u3068\u3067\u3001\u305D\u308C\u3092\u57FA\u6E96\
  \u306B\u8272\u3005\u306A\u51E6\u7406\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.515769-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u306E\u63A2\u3057\u65B9\u3068\u306F\
  \uFF1F \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u6587\u5B57\u5217\u306E\u9577\
  \u3055\u304C\u5206\u304B\u308B\u3053\u3068\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\
  \u3001\u30EB\u30FC\u30D7\u306E\u5236\u5FA1\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u51E6\
  \u7406\u306A\u3069\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u5177\u4F53\u7684\u306A\
  \u9577\u3055\u3092\u77E5\u308B\u3053\u3068\u3067\u3001\u305D\u308C\u3092\u57FA\u6E96\
  \u306B\u8272\u3005\u306A\u51E6\u7406\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why?
文字列の長さの探し方とは？
プログラミングで文字列の長さが分かることは、入力の検証、ループの制御、テキストの処理などに不可欠です。具体的な長さを知ることで、それを基準に色々な処理が可能になります。

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
