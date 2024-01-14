---
title:                "Haskell: ランダムな数字を生成する"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

```Haskell
　　　　　import System.Random
　　　　　
　　　　　main = do
　　　　　　gen <- getStdGen
　　　　　　print (take 5 (randomRs (1,10::Int) gen))
```

ランダムな数値生成というと、多くの人はギャンブルやゲームを思い浮かべるかもしれませんが、プログラミングでもランダムな数値が必要な場合があります。例えば、ゲームの開発や乱数を用いて確率的なアルゴリズムを構築する際には、ランダムな数値を生成する必要があります。また、ランダムな数値を用いてテストデータを生成することで、プログラムの動作を確認することもできます。

## 使い方

ランダムな数値を生成するには、Haskellの標準ライブラリであるSystem.Randomを用います。まず、このライブラリをインポートします。次に、getStdGenを使ってジェネレーターを取得し、randomRs関数を使って指定した範囲のランダムな数値を生成します。例として、1から10までのランダムな整数を5つ生成するプログラムは上記のようになります。

```
[6,8,1,4,10]
```

上記のように、毎回実行する度に異なる結果が得られることを確認してください。

## 深堀り
ランダムな数値の生成アルゴリズムには様々な種類がありますが、Haskellでは標準ライブラリのSystem.Randomに実装されている線形合同法 (linear congruential method) を用いています。これは、線形合同法を使用することで、簡単にランダムな数値を生成できるようになります。

ただし、このアルゴリズムは偏りがあるという問題があり、大きな数値や特定の値が多く生成される傾向があります。そのため、より高品質のランダム数値を生成したい場合は、外部ライブラリを使ったり、独自のアルゴリズムを実装する必要があります。

## 参考リンク
- [HaskellのSystem.Random](https://www.haskell.org/documentation/stdlib/Random.html)
- [線形合同法についての解説](https://www.quora.com/Why-is-the-linear-congruent-method-of-generating-pseudorandom-numbers-small-safe-small)
- [外部ライブラリのrandomを使用したランダム数値の生成方法](https://www.vex.net/~trebla/haskell/random.xhtml)

## 関連リンク
- [Haskell公式ドキュメンテーション](https://www.haskell.org/documentation/)
- [アルゴリズムの解説サイト「クォーラ」](https://www.quora.com/)