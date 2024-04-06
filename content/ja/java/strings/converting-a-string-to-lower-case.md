---
date: 2024-01-20 17:38:47.827596-07:00
description: "How to: (\u65B9\u6CD5) Java\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.826373-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Java\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u63DB\u3059\u308B\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## How to: (方法)
Javaで文字列を小文字に変換する簡単な例を見てみましょう。

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Kon'nichiwa, SEKAI!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```
出力:
```
kon'nichiwa, sekai!
```

## Deep Dive (掘り下げ)
Javaでは、`String` クラスの `toLowerCase()` メソッドで小文字変換を行います。初期のJavaバージョンから存在し、多言語対応を強化するために何度も改善されてきました。

**代替手法:** `toLowerCase()` にはロケールを指定できるオーバーロードがあります。例えば、`toLowerCase(Locale.ROOT)` は言語環境に依存しない結果を返すので、言語に敏感なデータで使われます。

**実装の詳細:** 内部的に `toLowerCase()` は文字コード表を使い、各文字を対応する小文字にマッピングします。Unicode規格に従い、特定言語の追加ルールを適用することもあります。

## See Also (関連情報)
- [Java String toLowerCase() Method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Locale-Specific Lowercase Conversion](https://docs.oracle.com/javase/tutorial/i18n/locale/toLowerCase.html)
