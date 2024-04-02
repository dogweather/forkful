---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:55.829288-07:00
description: "\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\
  \u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u4FDD\
  \u3064\u3053\u3068\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u3053\u306E\u4E00\u822C\u7684\u306A\
  \u6587\u5B57\u5217\u64CD\u4F5C\u30BF\u30B9\u30AF\u306F\u3001\u6163\u7FD2\u3084\u6587\
  \u6CD5\u7684\u306A\u6B63\u78BA\u3055\u306B\u5F93\u3063\u3066\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3084\u30BF\u30A4\u30C8\u30EB\u3092\u8868\u793A\u7528\u306B\u6E96\u5099\u3059\
  \u308B\u306A\u3069\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u30C6\
  \u30AD\u30B9\u30C8\u3092\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3059\u308B\u306E\u306B\
  \u5F79\u7ACB\u3061\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.921348-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\
  \u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u4FDD\
  \u3064\u3053\u3068\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u3053\u306E\u4E00\u822C\u7684\u306A\
  \u6587\u5B57\u5217\u64CD\u4F5C\u30BF\u30B9\u30AF\u306F\u3001\u6163\u7FD2\u3084\u6587\
  \u6CD5\u7684\u306A\u6B63\u78BA\u3055\u306B\u5F93\u3063\u3066\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3084\u30BF\u30A4\u30C8\u30EB\u3092\u8868\u793A\u7528\u306B\u6E96\u5099\u3059\
  \u308B\u306A\u3069\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u30C6\
  \u30AD\u30B9\u30C8\u3092\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3059\u308B\u306E\u306B\
  \u5F79\u7ACB\u3061\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 何となぜ？
文字列の最初の文字を大文字に変更し、残りを小文字に保つことで文字列を大文字にすることを指します。この一般的な文字列操作タスクは、慣習や文法的な正確さに従ってユーザー名やタイトルを表示用に準備するなど、アプリケーションでテキストをフォーマットするのに役立ちます。

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
