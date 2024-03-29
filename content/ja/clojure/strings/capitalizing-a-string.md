---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:59.835683-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u5217\u306F\
  \u5909\u66F4\u3057\u306A\u3044\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7279\u306B\u540D\u524D\u3084\u5834\
  \u6240\u306E\u30C7\u30FC\u30BF\u3092\u4E00\u8CAB\u6027\u3092\u6301\u305F\u305B\u308B\
  \u305F\u3081\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\
  \u30D5\u30A7\u30FC\u30B9\u306E\u6587\u6CD5\u898F\u5247\u306B\u6E96\u62E0\u3059\u308B\
  \u305F\u3081\u306B\u3001\u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u5316\u3092\u983B\
  \u7E41\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.534585-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u305D\u306E\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u5217\u306F\
  \u5909\u66F4\u3057\u306A\u3044\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7279\u306B\u540D\u524D\u3084\u5834\
  \u6240\u306E\u30C7\u30FC\u30BF\u3092\u4E00\u8CAB\u6027\u3092\u6301\u305F\u305B\u308B\
  \u305F\u3081\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\
  \u30D5\u30A7\u30FC\u30B9\u306E\u6587\u6CD5\u898F\u5247\u306B\u6E96\u62E0\u3059\u308B\
  \u305F\u3081\u306B\u3001\u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u5316\u3092\u983B\
  \u7E41\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を大文字化するとは、その文字列の最初の文字を大文字に変更し、残りの文字列は変更しないことを意味します。プログラマーは、特に名前や場所のデータを一貫性を持たせるため、またはユーザーインターフェースの文法規則に準拠するために、文字列の大文字化を頻繁に行います。

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
