---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:26.917168-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
乱数生成とは、予測できない数列を作ることです。プログラメーシングにおいて、ゲーム、シミュレーション、セキュリティなどで必要になります。

## How to: (方法)
Haskellで乱数を生成するには、`random`ライブラリを使います。下記が基本です：

```haskell
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn $ "Your random number: " ++ show randNumber
```

実行結果例：

```
Your random number: 7
```

乱数リスト：

```haskell
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let randNumbers = take 5 $ randomRs (1, 100) gen :: [Int]
    putStrLn $ "Your random numbers: " ++ show randNumbers
```

出力例：

```
Your random numbers: [53, 29, 44, 8, 91]
```

## Deep Dive (深掘り)
乱数生成の方法は歴史的にいくつかありますが、古典的なのは線形合同法です。Haskellでは、System.Randomモジュールの標準ジェネレータはミキサー型(Mersenne Twister)に基づいています。

他にも、Crypto.Randomなどの安全でない乱数と暗号学的に安全な乱数を提供するライブラリがあります。

システムによっては、`/dev/random`や`/dev/urandom`から直接乱数を取得できることもありますが、移植性は低下します。

## See Also (関連リンク)
- Haskell `random` ライブラリのドキュメント: [Hackage: random](https://hackage.haskell.org/package/random)
- 暗号学的に安全な乱数生成: [Hackage: Crypto.Random](https://hackage.haskell.org/package/crypto-api)