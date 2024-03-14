---
date: 2024-01-20 17:58:33.074374-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u5185\u306E\u7279\u5B9A\u306E\u6587\u5B57\u3084\u30EF\u30FC\u30C9\u3092\
  \u898B\u3064\u3051\u3066\u3001\u4ED6\u306E\u3082\u306E\u3067\u7F6E\u304D\u63DB\u3048\
  \u308B\u51E6\u7406\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30C7\u30FC\u30BF\u306E\u4FEE\u6B63\u3084\u30B3\u30FC\u30C9\u66F4\u65B0\u3001\u3042\
  \u308B\u3044\u306F\u30E6\u30FC\u30B6\u306E\u5165\u529B\u306E\u30D0\u30EA\u30C7\u30FC\
  \u30B7\u30E7\u30F3\u3092\u884C\u3046\u305F\u3081\u306B\u3001\u3053\u308C\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.924550-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u5185\u306E\u7279\u5B9A\u306E\u6587\u5B57\u3084\u30EF\u30FC\u30C9\u3092\
  \u898B\u3064\u3051\u3066\u3001\u4ED6\u306E\u3082\u306E\u3067\u7F6E\u304D\u63DB\u3048\
  \u308B\u51E6\u7406\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30C7\u30FC\u30BF\u306E\u4FEE\u6B63\u3084\u30B3\u30FC\u30C9\u66F4\u65B0\u3001\u3042\
  \u308B\u3044\u306F\u30E6\u30FC\u30B6\u306E\u5165\u529B\u306E\u30D0\u30EA\u30C7\u30FC\
  \u30B7\u30E7\u30F3\u3092\u884C\u3046\u305F\u3081\u306B\u3001\u3053\u308C\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキスト検索と置換は、文字列内の特定の文字やワードを見つけて、他のもので置き換える処理です。プログラマーは、データの修正やコード更新、あるいはユーザの入力のバリデーションを行うために、これを使用します。

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
