---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:55.829288-07:00
description: "\u65B9\u6CD5 Java\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  \u3001\u4E00\u5EA6\u306B\u6587\u5B57\u5217\u5168\u4F53\u3092\u5927\u6587\u5B57\u306B\
  \u3059\u308B\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\
  \u307E\u305B\u3093\u304C\u3001\u7D44\u307F\u8FBC\u307F\u306E\u30E1\u30BD\u30C3\u30C9\
  \u3092\u7D44\u307F\u5408\u308F\u305B\u308B\u3053\u3068\u3067\u3053\u308C\u3092\u5B9F\
  \u73FE\u3067\u304D\u307E\u3059\u3002\u3082\u3063\u3068\u6D17\u7DF4\u3055\u308C\u305F\
  \u30CB\u30FC\u30BA\u306E\u305F\u3081\u306B\u3001Apache Commons Lang\u306E\u3088\u3046\
  \u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001\u76F4\u63A5\u7684\u306A\u89E3\u6C7A\u7B56\u3092\u63D0\u4F9B\u3057\u307E\
  \u3059\u3002 #."
lastmod: '2024-03-13T22:44:41.921348-06:00'
model: gpt-4-0125-preview
summary: "Java\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001\u4E00\u5EA6\
  \u306B\u6587\u5B57\u5217\u5168\u4F53\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u76F4\
  \u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u305B\u3093\
  \u304C\u3001\u7D44\u307F\u8FBC\u307F\u306E\u30E1\u30BD\u30C3\u30C9\u3092\u7D44\u307F\
  \u5408\u308F\u305B\u308B\u3053\u3068\u3067\u3053\u308C\u3092\u5B9F\u73FE\u3067\u304D\
  \u307E\u3059\u3002\u3082\u3063\u3068\u6D17\u7DF4\u3055\u308C\u305F\u30CB\u30FC\u30BA\
  \u306E\u305F\u3081\u306B\u3001Apache Commons Lang\u306E\u3088\u3046\u306A\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001\u76F4\
  \u63A5\u7684\u306A\u89E3\u6C7A\u7B56\u3092\u63D0\u4F9B\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法
Javaの標準ライブラリは、一度に文字列全体を大文字にする直接的な方法を提供していませんが、組み込みのメソッドを組み合わせることでこれを実現できます。もっと洗練されたニーズのために、Apache Commons Langのようなサードパーティーライブラリは、直接的な解決策を提供します。

### Javaの組み込みメソッドを使用する
外部ライブラリなしで文字列を大文字化するには、文字列を単語に分割し、各単語の最初の文字を大文字にしてから再結合します。ここに簡単なアプローチがあります：

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // 出力: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

このコードスニペットは、文字列全体を小文字に変換し、各文字を繰り返し処理して、各単語の最初の文字を大文字に変換します。空白、ピリオド、アポストロフィを単語のセパレーターとして考慮しています。

### Apache Commons Langを使用する
Apache Commons Langライブラリは、さまざまなエッジケースやデリミターを処理する`WordUtils.capitalizeFully()`メソッドで、よりエレガントな解決策を提供します：

```java
// 依存関係を追加: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // 出力: "Hello, World!"
    }
}
```

このメソッドを使用するには、Apache Commons Langライブラリをプロジェクトに追加する必要があります。このライブラリの方法は、各単語の最初の文字を大文字化するだけでなく、各単語の残りの文字を小文字に変換し、文字列全体で一貫した大文字化パターンを保証します。
