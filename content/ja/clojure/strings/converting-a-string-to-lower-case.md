---
date: 2024-01-20 17:38:17.903069-07:00
description: "How to: (\u65B9\u6CD5) Clojure\u3067\u306F`clojure.string/lower-case`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\
  \u3048\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.489132-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Clojure\u3067\u306F`clojure.string/lower-case`\u95A2\u6570\
  \u3092\u4F7F\u3063\u3066\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u3048\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
