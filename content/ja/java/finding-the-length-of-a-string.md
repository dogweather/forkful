---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:43.107852-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/finding-the-length-of-a-string.md"
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