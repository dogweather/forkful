---
aliases:
- /ja/clojure/searching-and-replacing-text/
date: 2024-01-20 17:57:37.419206-07:00
description: "\u30B3\u30FC\u30C9\u306B\u304A\u3051\u308B\u30C6\u30AD\u30B9\u30C8\u306E\
  \u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\
  \u898B\u3064\u3051\u3066\u3001\u305D\u308C\u3092\u5225\u306E\u6587\u5B57\u5217\u306B\
  \u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30B3\u30FC\
  \u30C9\u306E\u30D0\u30B0\u4FEE\u6B63\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3001\u3042\u308B\u3044\u306F\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\
  \u306B\u983B\u7E41\u306B\u884C\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.589901
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C9\u306B\u304A\u3051\u308B\u30C6\u30AD\u30B9\u30C8\u306E\
  \u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\
  \u898B\u3064\u3051\u3066\u3001\u305D\u308C\u3092\u5225\u306E\u6587\u5B57\u5217\u306B\
  \u5909\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u3001\u30B3\u30FC\
  \u30C9\u306E\u30D0\u30B0\u4FEE\u6B63\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3001\u3042\u308B\u3044\u306F\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\
  \u306B\u983B\u7E41\u306B\u884C\u308F\u308C\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
コードにおけるテキストの検索と置換は、特定の文字列を見つけて、それを別の文字列に変えることです。これは、コードのバグ修正、リファクタリング、あるいは一貫性を保つために頻繁に行われます。

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
