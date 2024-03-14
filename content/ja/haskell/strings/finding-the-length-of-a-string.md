---
date: 2024-01-20 17:47:35.512595-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3068\u306F\u3001\
  \u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\u306E\u6570\
  \u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3084\u30C7\u30FC\u30BF\u691C\
  \u8A3C\u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u6700\u9069\u5316\u306A\u3069\
  \u69D8\u3005\u306A\u7406\u7531\u304B\u3089\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\
  \u6C42\u3081\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.170824-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3068\u306F\u3001\
  \u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\u306E\u6570\
  \u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u3084\u30C7\u30FC\u30BF\u691C\
  \u8A3C\u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u6700\u9069\u5316\u306A\u3069\
  \u69D8\u3005\u306A\u7406\u7531\u304B\u3089\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\
  \u6C42\u3081\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを知るとは、その文字列に含まれる文字の数を数えることです。プログラマーは、テキスト処理やデータ検証、パフォーマンス最適化など様々な理由から文字列の長さを求めます。

## How to: (方法)
```Haskell
main :: IO ()
main = do
    let str = "こんにちは"
    print $ length str  -- 文字列の長さを出力
```
サンプル出力:
```Haskell
5
```

## Deep Dive (掘り下げ)
Haskellでは、`length` 関数がリストの長さを返します。文字列も文字のリストだと考えられるため、`length` が使えます。過去には文字列操作の効率を上げるために他の関数やライブラリも開発されました。例えば、`Data.Text` パッケージではより効率的なテキスト処理が可能です。`length` はシンプルですが、大きな文字列でパフォーマンスの問題が生じることがあります。これは`length`がリスト全体を走査するからです。

## See Also (参照)
- [Haskell `length` documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:length)
- [Haskell Wiki: Performance](https://wiki.haskell.org/Performance)
- [`Data.Text` package](https://hackage.haskell.org/package/text)
- [Online Haskell Compiler](https://repl.it/languages/haskell)
