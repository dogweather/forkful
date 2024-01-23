---
title:                "文字列の連結"
date:                  2024-01-20T17:35:19.498266-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
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
