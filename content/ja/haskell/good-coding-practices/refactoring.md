---
date: 2024-01-26 01:38:26.003874-07:00
description: "\u65B9\u6CD5\uFF1A \u304A\u6C17\u306B\u5165\u308A\u306E\u66F2\u3088\u308A\
  \u3082\u7E70\u308A\u8FD4\u3055\u308C\u308BHaskell\u30B3\u30FC\u30C9\u306E\u584A\u304C\
  \u3042\u308B\u3068\u3057\u307E\u3059\u3002\u305D\u306E\u30B3\u30FC\u30C9\u3092\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3059\
  \u308B\u65B9\u6CD5\u3092\u7C21\u5358\u306B\u898B\u3066\u307F\u307E\u3057\u3087\u3046\
  \u3002 \u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u524D\uFF1A."
lastmod: '2024-03-13T22:44:42.196790-06:00'
model: gpt-4-0125-preview
summary: "\u304A\u6C17\u306B\u5165\u308A\u306E\u66F2\u3088\u308A\u3082\u7E70\u308A\
  \u8FD4\u3055\u308C\u308BHaskell\u30B3\u30FC\u30C9\u306E\u584A\u304C\u3042\u308B\u3068\
  \u3057\u307E\u3059\u3002\u305D\u306E\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u3092\u4F7F\
  \u3063\u3066\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3059\u308B\u65B9\u6CD5\
  \u3092\u7C21\u5358\u306B\u898B\u3066\u307F\u307E\u3057\u3087\u3046."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 方法：
お気に入りの曲よりも繰り返されるHaskellコードの塊があるとします。そのコードを関数を使ってリファクタリングする方法を簡単に見てみましょう。

リファクタリング前：

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

少しリファクタリングした後：

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- サンプル出力：
-- Customer: Alice
-- Total: $42.00
-- Item: Haskell Programming Guide
```

ご覧の通り、共通パターンを別の `printDetail` 関数に抽出することで、繰り返しを避け、`printInvoice`をより明確で管理しやすくします。

## ディープダイブ
80年代後半にHaskellが登場したとき、関数型パラダイムがコーディングの慣習に新鮮な空気をもたらすことが明らかでした。早送りして、関数が第一級の市民であり、強力な静的型システムを持つHaskellでは、リファクタリングが特にエレガントです。コンパイラが背中を押してくれるので、アプリを壊すことを恐れずにリファクタリングができます。

手動リファクタリングの代替方法には、自動化ツールの使用が含まれる場合がありますが、Haskellの関数型の性質と型の安全性により、他の言語と比較してこれがあまり一般的ではないこともあります。実装において、リファクタリングをスムーズにするために、Haskellの高階関数、純粋性、不変性などの特徴を活用することが重要です。

「関数の抽出」のようなリファクタリングは一般的ですが、型システムのおかげで、「関数のインライン化」、「変数の名前変更」、「関数シグネチャの変更」も自信を持って行うことができます。Haskellの強力な型推論は、他の言語では見逃されることがあるエラーをときにキャッチすることができます。

## 参考
Haskellでのリファクタリングに深く潜りたい場合は、Martin Fowlerの「Refactoring: Improving the Design of Existing Code」を読んでみてください。ここでは、その概念が普遍的に適用されます。Haskellコードを改善するための自動的なヒントを提供するhlintツールをチェックしてください。また、コミュニティの洞察とさらなる読書のためにHaskell wiki (https://wiki.haskell.org/Refactoring)を訪れてください。
