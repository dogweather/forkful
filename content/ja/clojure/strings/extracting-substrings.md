---
date: 2024-01-20 17:45:30.492167-07:00
description: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u3068\u306F\u3001\u5927\
  \u304D\u306A\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\u5206\u3060\u3051\
  \u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u30C7\
  \u30FC\u30BF\u89E3\u6790\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3001\u30ED\u30AE\
  \u30F3\u30B0\u306A\u3069\u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u30BF\u30B9\u30AF\u3067\u5FC5\u8981\u3068\u306A\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.542846-06:00'
model: gpt-4-1106-preview
summary: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u3068\u306F\u3001\u5927\
  \u304D\u306A\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\u5206\u3060\u3051\
  \u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306F\u30C7\
  \u30FC\u30BF\u89E3\u6790\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3001\u30ED\u30AE\
  \u30F3\u30B0\u306A\u3069\u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u30BF\u30B9\u30AF\u3067\u5FC5\u8981\u3068\u306A\u308A\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## What & Why?
部分文字列の抽出とは、大きな文字列から必要な部分だけを取り出すことです。これはデータ解析、テキスト処理、ロギングなど多くのプログラミングタスクで必要となります。

## How to:
Clojureでは `subs` 関数を用いて部分文字列を簡単に取り出せます。

```Clojure
(def full-string "Clojureは素晴らしい！")

;; 文字列の6文字目から11文字目を取り出す
(println (subs full-string 5 12)) ; => "は素晴らし"

;; 文字列の最初から5文字目までを取り出す
(println (subs full-string 0 5)) ; => "Clojur"
```

これらコードは、指定した範囲の文字列を表示します。

## Deep Dive
Clojureの `subs` 関数はJavaの `substring` メソッドに基づいており、`0` からインデックスを数え始め、開始インデックスを含み、終了インデックスを含まない範囲を返します。他の言語では `slice` など別の名前で似たような操作ができます。ClojureがJavaのVM上で動く関係で、`subs` 関数もJavaの文字列処理の効率性を享受しています。ただし、不要なメモリ消費を避けるため、大きな文字列から小さな部分を抽出する際は注意が必要です。

## See Also
- Clojureの公式ドキュメント [`subs` function](https://clojuredocs.org/clojure.core/subs)
- Javaの [`substring` method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
