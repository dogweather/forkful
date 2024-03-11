---
date: 2024-01-20 17:35:19.498266-07:00
description: "\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\u3068\u306F\u3001\uFF12\
  \u3064\u4EE5\u4E0A\u306E\u6587\u5B57\u5217\u3092\u4E00\u3064\u306B\u3064\u306A\u3052\
  \u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\
  \u306A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\u6210\u3057\u305F\u308A\u3001\u30C7\
  \u30FC\u30BF\u306E\u5F62\u5F0F\u3092\u6574\u3048\u305F\u308A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.516915-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\u3068\u306F\u3001\uFF12\
  \u3064\u4EE5\u4E0A\u306E\u6587\u5B57\u5217\u3092\u4E00\u3064\u306B\u3064\u306A\u3052\
  \u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\
  \u306A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\u6210\u3057\u305F\u308A\u3001\u30C7\
  \u30FC\u30BF\u306E\u5F62\u5F0F\u3092\u6574\u3048\u305F\u308A\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列を連結するとは、２つ以上の文字列を一つにつなげることです。これにより、動的なメッセージを作成したり、データの形式を整えたりします。

## How to: (方法)

```java
public class StringConcatenation {
    public static void main(String[] args) {
        String hello = "こんにちは、";
        String world = "世界！";
        String greeting = hello + world; // 文字列を+演算子で連結
        
        System.out.println(greeting); // 出力: こんにちは、世界！

        String age = "年齢: ";
        int myAge = 25;
        String ageStatement = age + myAge; // 文字列と数値を連結
        
        System.out.println(ageStatement); // 出力: 年齢: 25

        String builderExample = String.join(", ", "Java", "Python", "C++"); // String.joinを使って連結
        
        System.out.println(builderExample); // 出力: Java, Python, C++
    }
}
```

## Deep Dive (深掘り)

文字列の連結はJavaの初期バージョンからありますが、パフォーマンスは時間と共に向上しています。`+`演算子は内部で`StringBuilder`を使って最適化され、多くの連結がある場合でも効率的です。しかし、扱う文字列が非常に多い場合、`StringBuilder`か`StringBuffer`クラスを直接使ったほうが速いことがあります。

また、Java 8では`String.join`が登場し、複数の文字列を区切り文字で簡単に連結できるようになりました。さらにJava 11では`String`クラスに`repeat`メソッドが追加され、同じ文字列を繰り返して連結する要件が簡単に実装できるようになりました。

## See Also (関連リンク)

- Oracle Java Documentation on Strings: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html
- Java Performance Tuning Guide – String Concatenation: https://www.javaperformancetuning.com/articles/stringconcatenation.shtml
- StringBuilder vs StringBuffer: https://www.baeldung.com/java-stringbuilder-stringbuffer
