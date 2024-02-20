---
date: 2024-01-26 03:45:06.441118-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u305D\u308C\u3089\
  \u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u3084\u6307\u5B9A\u3055\u308C\u305F\u5C0F\
  \u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u306B\u8ABF\u6574\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7CBE\u5EA6\u3092\u5236\
  \u5FA1\u3059\u308B\u305F\u3081\u3001\u30E6\u30FC\u30B6\u30FC\u3078\u306E\u8868\u793A\
  \u3092\u8ABF\u6574\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u6D6E\u52D5\u5C0F\
  \u6570\u70B9\u6F14\u7B97\u306E\u8A08\u7B97\u30B3\u30B9\u30C8\u3092\u524A\u6E1B\u3059\
  \u308B\u305F\u3081\u306B\u3001\u6570\u5024\u3092\u4E38\u3081\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.322121
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u305D\u308C\u3089\
  \u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u3084\u6307\u5B9A\u3055\u308C\u305F\u5C0F\
  \u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u306B\u8ABF\u6574\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7CBE\u5EA6\u3092\u5236\
  \u5FA1\u3059\u308B\u305F\u3081\u3001\u30E6\u30FC\u30B6\u30FC\u3078\u306E\u8868\u793A\
  \u3092\u8ABF\u6574\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u6D6E\u52D5\u5C0F\
  \u6570\u70B9\u6F14\u7B97\u306E\u8A08\u7B97\u30B3\u30B9\u30C8\u3092\u524A\u6E1B\u3059\
  \u308B\u305F\u3081\u306B\u3001\u6570\u5024\u3092\u4E38\u3081\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？

数値を丸めるとは、それらを最も近い整数や指定された小数点以下の桁に調整することです。プログラマーは、精度を制御するため、ユーザーへの表示を調整するため、または浮動小数点演算の計算コストを削減するために、数値を丸めます。

## 方法：

Haskellは丸め操作のために、`Prelude`から`round`、`ceiling`、`floor`、`truncate`関数を使用します。

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- 特定の小数点以下の桁に丸める機能はPreludeにはありません。
  -- こちらカスタム関数です：
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## 詳細解説

歴史的に、丸めは数値解析やコンピュータサイエンスにおいて重要であり、特にIEEE 754によって浮動小数点表現が標準化される前に、計算における誤差蓄積を最小限にするために不可欠です。

何に丸めるか？`round`は最も近い整数—上または下—に丸めます。`ceiling`と`floor`はそれぞれ常に最も近い整数に上または下へ丸め、`truncate`は単に小数点以下を落とします。

これらの関数に代わるものとしては、私たちの`roundTo`のようなカスタムロジックや、もっと複雑な要件に対応するためにライブラリ（Data.Fixedのような）を取り込むことが挙げられます。

`round`での半分の場合の取り扱いによって、Haskellが予期せぬ結果を出すことがあるので注意してください（最も近い偶数に丸める）。

## 参照

- 丸め機能のためのHaskell Preludeドキュメント：https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- 浮動小数点算術についてのHaskell Wiki：https://wiki.haskell.org/Floating_point_arithmetic
- 多くの言語で浮動小数点がどのように扱われているかについてのIEEE 754-2008標準：https://ieeexplore.ieee.org/document/4610935
