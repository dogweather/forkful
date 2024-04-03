---
date: 2024-01-20 17:58:33.074374-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.924550-06:00'
model: gpt-4-1106-preview
summary: .
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
