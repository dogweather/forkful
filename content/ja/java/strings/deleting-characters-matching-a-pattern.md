---
date: 2024-01-20 17:42:35.779542-07:00
description: "How to: (\u65B9\u6CD5) Java\u3067\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u3092\u884C\u3046\u6B74\u53F2\u306F\u53E4\u304F\u3001`java.util.regex`\
  \ \u30D1\u30C3\u30B1\u30FC\u30B8\u306F Java 1.4 \u4EE5\u964D\u3067\u5229\u7528\u53EF\
  \u80FD\u3067\u3059\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u3001`Pattern`\
  \ \u30AF\u30E9\u30B9\u3068 `Matcher` \u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\
  \u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u30B7\u30F3\u30D7\u30EB\u306A\
  \u7F6E\u63DB\u306E\u5834\u5408\u306F `String` \u306E\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.822944-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Java\u3067\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u3092\u884C\u3046\u6B74\u53F2\u306F\u53E4\u304F\u3001`java.util.regex` \u30D1\
  \u30C3\u30B1\u30FC\u30B8\u306F Java 1.4 \u4EE5\u964D\u3067\u5229\u7528\u53EF\u80FD\
  \u3067\u3059\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u3001`Pattern`\
  \ \u30AF\u30E9\u30B9\u3068 `Matcher` \u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\
  \u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u30B7\u30F3\u30D7\u30EB\u306A\
  \u7F6E\u63DB\u306E\u5834\u5408\u306F `String` \u306E `replaceAll` \u30E1\u30BD\u30C3\
  \u30C9\u304C\u4FBF\u5229\u3067\u3059\u3002\u5B9F\u88C5\u306E\u8A73\u7D30\u3067\u306F\
  \u3001\u30D1\u30BF\u30FC\u30F3\u306E\u30B3\u30F3\u30D1\u30A4\u30EB\u3084\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u306E\u969B\u306E\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306B\
  \u6CE8\u610F\u3092\u6255\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\u3053\
  \u3053\u3067\u306F\u3001\u30D1\u30BF\u30FC\u30F3\u306F\u30D7\u30EA\u30B3\u30F3\u30D1\
  \u30A4\u30EB\u3055\u308C\u3001\u518D\u5229\u7528\u3059\u308B\u3053\u3068\u3067\u52B9\
  \u7387\u304C\u5411\u4E0A\u3057\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
```java
public class PatternMatcher {
    public static void main(String[] args) {
        String originalText = "Hello123 World! Have a great2023 day!";
        String modifiedText = originalText.replaceAll("\\d+", ""); // 数字を削除する正規表現
        System.out.println(modifiedText);
    }
}
```
実行結果：
```
Hello World! Have a great day!
```
この例では、`replaceAll`メソッドと正規表現 `\\d+` を使って数字を削除しています。

## Deep Dive (深掘り)
Javaでパターンマッチングを行う歴史は古く、`java.util.regex` パッケージは Java 1.4 以降で利用可能です。代替手段としては、`Pattern` クラスと `Matcher` クラスを利用する方法がありますが、シンプルな置換の場合は `String` の `replaceAll` メソッドが便利です。実装の詳細では、パターンのコンパイルやマッチングの際のパフォーマンスに注意を払う必要があります。ここでは、パターンはプリコンパイルされ、再利用することで効率が向上します。

## See Also (関連情報)
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Regular Expressions Tutorial](https://www.regular-expressions.info/java.html)
- [Oracle Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
