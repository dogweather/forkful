---
title:                "数値の丸め処理"
aliases:
- /ja/haskell/rounding-numbers/
date:                  2024-01-26T03:45:06.441118-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/rounding-numbers.md"
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
