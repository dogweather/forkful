---
date: 2024-01-20 17:58:33.074374-07:00
description: "How to: (\u3084\u308A\u65B9) \u30C6\u30AD\u30B9\u30C8\u3092\u691C\u7D22\
  \u3057\u3001\u7F6E\u63DB\u3059\u308B\u6A5F\u80FD\u306F\u591A\u304F\u306E\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u5099\u308F\u3063\u3066\u3044\u307E\
  \u3059\u304C\u3001Java\u3067\u306E\u3053\u306E\u6A5F\u80FD\u306F\u3001`java.lang.String`\u30AF\
  \u30E9\u30B9\u306B\u5B9F\u88C5\u3055\u308C\u3066\u3044\u307E\u3059\u3002`replace()`,\
  \ `replaceAll()`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.824298-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30C6\u30AD\u30B9\u30C8\u3092\u691C\u7D22\u3057\u3001\
  \u7F6E\u63DB\u3059\u308B\u6A5F\u80FD\u306F\u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u5099\u308F\u3063\u3066\u3044\u307E\u3059\u304C\
  \u3001Java\u3067\u306E\u3053\u306E\u6A5F\u80FD\u306F\u3001`java.lang.String`\u30AF\
  \u30E9\u30B9\u306B\u5B9F\u88C5\u3055\u308C\u3066\u3044\u307E\u3059\u3002`replace()`,\
  \ `replaceAll()`, `replaceFirst()`\u304C\u305D\u306E\u4F8B\u3067\u3059\u3002`replaceAll()`\u3067\
  \u306F\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3067\u304D\u3001\u5E83\u7BC4\u306A\
  \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u304C\u53EF\u80FD\u306B\u306A\
  \u308A\u307E\u3059\u3002\u305F\u3060\u3057\u3001\u6B63\u898F\u8868\u73FE\u306B\u306F\
  \u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u30B3\u30B9\u30C8\u304C\u4F34\u3044\
  \u307E\u3059\u3002\u4E00\u65B9\u3067\u3001`replace()`\u306F\u5358\u4E00\u306E\u6587\
  \u5B57\u5217\u3084\u6587\u5B57\u7F6E\u63DB\u306B\u7528\u3044\u3089\u308C\u3001\u6B63\
  \u898F\u8868\u73FE\u306F\u4F7F\u3044\u307E\u305B\u3093\u3002\u3053\u308C\u306B\u3088\
  \u308A\u3001\u9AD8\u901F\u304B\u3064\u30B7\u30F3\u30D7\u30EB\u306A\u7F6E\u63DB\u304C\
  \u884C\u3048\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (やり方)
```java
public class SearchReplaceExample {
    public static void main(String[] args) {
        String originalText = "こんにちは、Java。さようなら、Java。";
        String searchText = "Java";
        String replaceText = "世界";
        
        String replacedText = originalText.replace(searchText, replaceText);
        System.out.println(replacedText);
    }
}
```
出力:
```
こんにちは、世界。さようなら、世界。
```

## Deep Dive (詳細情報)
テキストを検索し、置換する機能は多くのプログラミング言語に備わっていますが、Javaでのこの機能は、`java.lang.String`クラスに実装されています。`replace()`, `replaceAll()`, `replaceFirst()`がその例です。`replaceAll()`では正規表現を使用でき、広範なパターンマッチングが可能になります。ただし、正規表現にはパフォーマンスのコストが伴います。一方で、`replace()`は単一の文字列や文字置換に用いられ、正規表現は使いません。これにより、高速かつシンプルな置換が行えます。

過去にはApache Commons Langなどのサードパーティライブラリが使用されていましたが、Javaの標準ライブラリが進化に伴い、これらのライブラリの使用は減少しています。

## See Also (関連情報)
- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html) (Java Stringクラス文書)
- [Pattern Class in Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html) (JavaでのPatternクラス)
- [Oracle Regex Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/) (Oracle正規表現チュートリアル)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/) (Apache Commons Langライブラリ)

Note: Web resources are linked to English pages as direct Japanese translations may not exist. Readers might need to use translation services if they require information in Japanese.
