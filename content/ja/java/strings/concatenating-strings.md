---
date: 2024-01-20 17:35:19.498266-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.832202-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9023\u7D50\u306FJava\u306E\u521D\
  \u671F\u30D0\u30FC\u30B8\u30E7\u30F3\u304B\u3089\u3042\u308A\u307E\u3059\u304C\u3001\
  \u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306F\u6642\u9593\u3068\u5171\u306B\u5411\
  \u4E0A\u3057\u3066\u3044\u307E\u3059\u3002`+`\u6F14\u7B97\u5B50\u306F\u5185\u90E8\
  \u3067`StringBuilder`\u3092\u4F7F\u3063\u3066\u6700\u9069\u5316\u3055\u308C\u3001\
  \u591A\u304F\u306E\u9023\u7D50\u304C\u3042\u308B\u5834\u5408\u3067\u3082\u52B9\u7387\
  \u7684\u3067\u3059\u3002\u3057\u304B\u3057\u3001\u6271\u3046\u6587\u5B57\u5217\u304C\
  \u975E\u5E38\u306B\u591A\u3044\u5834\u5408\u3001`StringBuilder`\u304B`StringBuffer`\u30AF\
  \u30E9\u30B9\u3092\u76F4\u63A5\u4F7F\u3063\u305F\u307B\u3046\u304C\u901F\u3044\u3053\
  \u3068\u304C\u3042\u308A\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
