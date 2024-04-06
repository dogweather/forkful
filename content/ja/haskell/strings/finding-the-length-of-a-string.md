---
date: 2024-01-20 17:47:35.512595-07:00
description: "How to: (\u65B9\u6CD5) Haskell\u3067\u306F\u3001`length` \u95A2\u6570\
  \u304C\u30EA\u30B9\u30C8\u306E\u9577\u3055\u3092\u8FD4\u3057\u307E\u3059\u3002\u6587\
  \u5B57\u5217\u3082\u6587\u5B57\u306E\u30EA\u30B9\u30C8\u3060\u3068\u8003\u3048\u3089\
  \u308C\u308B\u305F\u3081\u3001`length` \u304C\u4F7F\u3048\u307E\u3059\u3002\u904E\
  \u53BB\u306B\u306F\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u52B9\u7387\u3092\u4E0A\u3052\
  \u308B\u305F\u3081\u306B\u4ED6\u306E\u95A2\u6570\u3084\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3082\u958B\u767A\u3055\u308C\u307E\u3057\u305F\u3002\u4F8B\u3048\u3070\u3001`Data.Text`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.097904-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Haskell\u3067\u306F\u3001`length` \u95A2\u6570\u304C\u30EA\
  \u30B9\u30C8\u306E\u9577\u3055\u3092\u8FD4\u3057\u307E\u3059\u3002\u6587\u5B57\u5217\
  \u3082\u6587\u5B57\u306E\u30EA\u30B9\u30C8\u3060\u3068\u8003\u3048\u3089\u308C\u308B\
  \u305F\u3081\u3001`length` \u304C\u4F7F\u3048\u307E\u3059\u3002\u904E\u53BB\u306B\
  \u306F\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u52B9\u7387\u3092\u4E0A\u3052\u308B\u305F\
  \u3081\u306B\u4ED6\u306E\u95A2\u6570\u3084\u30E9\u30A4\u30D6\u30E9\u30EA\u3082\u958B\
  \u767A\u3055\u308C\u307E\u3057\u305F\u3002\u4F8B\u3048\u3070\u3001`Data.Text` \u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3067\u306F\u3088\u308A\u52B9\u7387\u7684\u306A\u30C6\u30AD\
  \u30B9\u30C8\u51E6\u7406\u304C\u53EF\u80FD\u3067\u3059\u3002`length` \u306F\u30B7\
  \u30F3\u30D7\u30EB\u3067\u3059\u304C\u3001\u5927\u304D\u306A\u6587\u5B57\u5217\u3067\
  \u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u554F\u984C\u304C\u751F\u3058\u308B\
  \u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u3053\u308C\u306F`length`\u304C\
  \u30EA\u30B9\u30C8\u5168\u4F53\u3092\u8D70\u67FB\u3059\u308B\u304B\u3089\u3067\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
