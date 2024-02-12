---
title:                "複素数の扱い方"
aliases:
- /ja/haskell/working-with-complex-numbers/
date:                  2024-01-26T04:41:46.453173-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

複素数は、実部と虚部から構成されており、エンジニアリング、物理学、信号処理など様々な計算領域において不可欠です。プログラマーは、実数では解けない方程式、例えば負の数の根を見つけるような計算にそれらを使用します。

## どのように：

Haskellは`Data.Complex`モジュールで複素数を扱います。ここに簡単な案内を示します：

```haskell
import Data.Complex

-- 二つの複素数を定義
let z1 = 3 :+ 4  -- これは 3 + 4i です
let z2 = 5 :+ (-2)  -- 5 - 2i です

-- 算術演算
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- 複素共役
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- 大きさと位相
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- 極座標から直交座標への変換、その逆も同様
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- z1 と同じ
```

上記のコードをGHCiでロードした後のサンプル出力は以下のようになります:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## 深掘り

複素数は16世紀に遡りますが、広く受け入れられたのはずっと後のことでした。Haskellを含む多くの言語は、複素数の算術に対してネイティブサポートを提供しており、基本的な数学を実装することなくこれらの数値を簡単に扱うことができます。

方法としては、独自の複素数型を構築するか、3Dグラフィックスのための四元数など、特定のドメイン向けのライブラリを使用することも考えられます。しかし、ほとんどの使用例において、Haskellの`Data.Complex`で十分です。

内部的には、`Data.Complex`は単に2つの`Float`または`Double`値を組み合わせたデータ型で、それぞれ実部と虚部を表しています。これはHaskellプラットフォームで複素数を扱うための直接的かつ効率的な方法です。

## 関連情報

Haskellでの複素数のさらなる情報については、これらのリソースを参照してください:

- 公式Haskell `Data.Complex`文書: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Haskellの数値型についての深い掘り下げ: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- Haskellでの高速フーリエ変換アルゴリズムの適用については: [Haskell FFT ライブラリ](https://hackage.haskell.org/package/fft)
