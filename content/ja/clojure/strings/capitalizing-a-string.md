---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:59.835683-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306FJVM\u8A00\u8A9E\u3067\u3042\u308B\u305F\
  \u3081\u3001Java\u306EString\u30E1\u30BD\u30C3\u30C9\u3092\u76F4\u63A5\u5229\u7528\
  \u3067\u304D\u307E\u3059\u3002\u3053\u3053\u306BClojure\u3067\u6587\u5B57\u5217\u3092\
  \u5927\u6587\u5B57\u5316\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.534585-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u306FJVM\u8A00\u8A9E\u3067\u3042\u308B\u305F\u3081\u3001Java\u306E\
  String\u30E1\u30BD\u30C3\u30C9\u3092\u76F4\u63A5\u5229\u7528\u3067\u304D\u307E\u3059\
  \u3002\u3053\u3053\u306BClojure\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\
  \u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
ClojureはJVM言語であるため、JavaのStringメソッドを直接利用できます。ここにClojureで文字列を大文字化する基本的な例を示します：

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojureには文字列を大文字化するための組み込み関数は含まれていませんが、示されているように、`clojure.string/upper-case`、`subs`、`str`関数を組み合わせることで、容易にこれを達成することができます。

もっと簡潔な解決策や、もっと複雑な文字列操作を扱う場合は、サードパーティーのライブラリに頼ることになるかもしれません。Clojureエコシステムで人気のあるライブラリの一つに`clojure.string`があります。しかし、私の最後の更新時点で、これはコアClojure機能で示されているものを超えて直接`capitalize`関数を提供していませんので、特に大文字化のために追加のライブラリを導入することなく、上記の方法が直接的なアプローチです。

Clojureで文字列を扱う際にJavaメソッドと交流がある場合、実際にはJavaの文字列を扱っており、必要に応じてJavaのStringメソッドの全範囲をClojureコードで直接活用できることを忘れないでください。
