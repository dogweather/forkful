---
date: 2024-01-20 17:38:47.827596-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u3059\u3079\u3066\u306E\u5927\u6587\u5B57\u3092\u5BFE\u5FDC\u3059\
  \u308B\u5C0F\u6587\u5B57\u306B\u5909\u3048\u308B\u51E6\u7406\u306E\u3053\u3068\u3067\
  \u3059\u3002\u691C\u7D22\u3001\u30BD\u30FC\u30C8\u3001\u4E00\u8CAB\u6027\u306E\u3042\
  \u308B\u30C7\u30FC\u30BF\u4FDD\u5B58\u306A\u3069\u306E\u305F\u3081\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3088\u304F\u3053\u306E\u64CD\u4F5C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.927288-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u3059\u3079\u3066\u306E\u5927\u6587\u5B57\u3092\u5BFE\u5FDC\u3059\
  \u308B\u5C0F\u6587\u5B57\u306B\u5909\u3048\u308B\u51E6\u7406\u306E\u3053\u3068\u3067\
  \u3059\u3002\u691C\u7D22\u3001\u30BD\u30FC\u30C8\u3001\u4E00\u8CAB\u6027\u306E\u3042\
  \u308B\u30C7\u30FC\u30BF\u4FDD\u5B58\u306A\u3069\u306E\u305F\u3081\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3088\u304F\u3053\u306E\u64CD\u4F5C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
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
