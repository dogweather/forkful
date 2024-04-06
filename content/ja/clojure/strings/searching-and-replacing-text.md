---
date: 2024-01-20 17:57:37.419206-07:00
description: "How to: (\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\
  \u7F6E\u63DB\u306F\u30A8\u30C7\u30A3\u30BF\u3084\u30EF\u30FC\u30C9\u30D7\u30ED\u30BB\
  \u30C3\u30B5\u304C\u6700\u521D\u306B\u5C0E\u5165\u3057\u305F\u6A5F\u80FD\u306E\u4E00\
  \u3064\u3067\u3059\u3002Clojure\u3067\u306F`clojure.string/replace`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.486862-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\
  \u306F\u30A8\u30C7\u30A3\u30BF\u3084\u30EF\u30FC\u30C9\u30D7\u30ED\u30BB\u30C3\u30B5\
  \u304C\u6700\u521D\u306B\u5C0E\u5165\u3057\u305F\u6A5F\u80FD\u306E\u4E00\u3064\u3067\
  \u3059\u3002Clojure\u3067\u306F`clojure.string/replace` \u95A2\u6570\u306A\u3069\
  \u3092\u4F7F\u3063\u3066\u30B7\u30F3\u30D7\u30EB\u306A\u7F6E\u63DB\u304C\u3067\u304D\
  \u307E\u3059\u3002\u6B63\u898F\u8868\u73FE\u306E\u5229\u7528\u304C\u53EF\u80FD\u3067\
  \u3001\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306E\u691C\u7D22\u3084\u7F6E\u63DB\
  \u3082\u5BFE\u5FDC\u3057\u3066\u3044\u307E\u3059\u3002\u521D\u671F\u306E\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u8A00\u8A9E\u3067\u306F\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u304C\
  \u9762\u5012\u3060\u3063\u305F\u304C\u3001Clojure\u306E\u3088\u3046\u306A\u73FE\u4EE3\
  \u8A00\u8A9E\u306F\u30D1\u30EF\u30D5\u30EB\u3067\u67D4\u8EDF\u306A\u6587\u5B57\u5217\
  \u64CD\u4F5C\u6A5F\u80FD\u3092\u5099\u3048\u3066\u3044\u307E\u3059\u3002\u4ED6\u306B\
  \u3082\u30C6\u30AD\u30B9\u30C8\u3092\u51E6\u7406\u3059\u308B\u65B9\u6CD5\u306F\u3042\
  \u308A\u307E\u3059\u304C\u3001`clojure.string`\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  \u7C21\u6F54\u306B\u66F8\u304F\u3053\u3068\u304C\u3067\u304D\u308B\u305F\u3081\u6700\
  \u3082\u666E\u53CA\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to: (方法)
```clojure
; 文字列置換の基本例
(replace-first "cat" "bat" "the cat sat on the mat")
; 出力: "the bat sat on the mat"

; 正規表現を使った文字列置換
(replace-first #"cat" "bat" "the cat sat on the cat mat")
; 出力: "the bat sat on the cat mat"

; 全部の該当箇所を置換する
(clojure.string/replace "the cat sat on the cat mat" #"cat" "bat")
; 出力: "the bat sat on the bat mat"
```

## Deep Dive (深いダイビング)
テキストの検索と置換はエディタやワードプロセッサが最初に導入した機能の一つです。Clojureでは`clojure.string/replace` 関数などを使ってシンプルな置換ができます。正規表現の利用が可能で、複雑なパターンの検索や置換も対応しています。初期のプログラム言語ではテキスト処理が面倒だったが、Clojureのような現代言語はパワフルで柔軟な文字列操作機能を備えています。他にもテキストを処理する方法はありますが、`clojure.string`ライブラリは簡潔に書くことができるため最も普及しています。

## See Also (参照)
- [Clojure Documentation for clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)
- [ClojureDocs - A community-powered documentation site](https://clojuredocs.org/)
