---
date: 2024-01-20 17:38:17.903069-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u4E0A\u3067\u5168\u3066\u306E\u5927\
  \u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4E00\u62EC\u3057\u3066\u5909\u3048\u308B\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u5927\u6587\u5B57\u30FB\u5C0F\
  \u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u3084\u3001\u30C7\u30FC\
  \u30BF\u306E\u6574\u5F62\u30FB\u6A19\u6E96\u5316\u306E\u969B\u306B\u5F79\u7ACB\u3061\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.167653-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u4E0A\u3067\u5168\u3066\u306E\u5927\
  \u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4E00\u62EC\u3057\u3066\u5909\u3048\u308B\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u5927\u6587\u5B57\u30FB\u5C0F\
  \u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\u691C\u7D22\u3084\u3001\u30C7\u30FC\
  \u30BF\u306E\u6574\u5F62\u30FB\u6A19\u6E96\u5316\u306E\u969B\u306B\u5F79\u7ACB\u3061\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、プログラム上で全ての大文字を小文字に一括して変えることです。これは、大文字・小文字を区別しない検索や、データの整形・標準化の際に役立ちます。

## How to: (方法)
Clojureでは`clojure.string/lower-case`関数を使って文字列を小文字に変えます。

```clojure
(require '[clojure.string :as str])

;; 文字列を小文字に変換
(str/lower-case "Hello, World!")
;; => "hello, world!"
```

シンプルですね。試してみてください。

## Deep Dive (掘り下げ)
Clojureでの小文字変換はJavaの`toLowerCase`を裏側で使います。これはUnicode標準に従っているため、多言語に対応しています。

以前のバージョンでは独自の実装だったこともありますが、標準化と効率のためJavaのメソッドが利用されています。

他の方法？`map`関数と`Character/toLowerCase`で一文字ずつ変換することもできますが、遅く不便です。

```clojure
(apply str (map #(Character/toLowerCase %) "Hello, World!"))
;; => "hello, world!"
```

普通は`clojure.string/lower-case`を使いましょう。

## See Also (関連情報)
- Clojureの公式ドキュメントの`clojure.string/lower-case`: https://clojuredocs.org/clojure.string/lower-case
- Unicodeの標準について: https://unicode.org/
- Javaの`toLowerCase`について: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
