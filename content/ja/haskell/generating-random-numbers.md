---
title:    "Haskell: 「ランダム数値の生成」"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することの魅力について言及します。Haskell言語を使ってプログラミングする際に、ランダムな数値がどのように役立つのかについて解説します。

## 使い方

Haskell言語でランダムな数値を生成する方法について説明します。下記のコードブロックを参考にしてください。

```Haskell
import System.Random

-- 0から10の範囲でランダムな数値を生成する
main = do
    gen <- getStdGen
    let randomNum = take 1 $ randomRs (0,10) gen
    print randomNum
```

`System.Random`ライブラリをインポートし、`getStdGen`関数を使用してランダム値のジェネレーターを取得します。その後、`randomRs`関数を使用して指定した範囲内でのランダムな数値を生成します。最後に`print`関数を使って結果を出力します。

実行すると、コードブロック内の`randomNum`変数にはランダムな数値が代入され、出力結果として表示されます。

## 深層へ

ランダムな数値を生成する際には、乱数の種を指定することが大切です。同じ種を使えば常に同じ結果が得られるため、プログラムのテストやデバッグ時に役立ちます。また、ジェネレーターを更新することで、異なる乱数を得ることもできます。

ランダムな数値を扱う際には、偏りやバイアスなどの問題があります。Haskellでは、`System.Random`モジュールに含まれる異なるアルゴリズムを使用することで、この問題を解決することができます。

## 関連情報

- [Haskellの公式ドキュメント](https://www.haskell.org/documentation/)
- [ランダムな数値を生成する方法](https://qiita.com/kkyouhei/items/59ea86b3c8d5f0d276ca)
- [Haskellで乱数を生成する方法](https://qiita.com/nwtgck/items/3410d6d99f9323eec426)