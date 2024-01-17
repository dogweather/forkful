---
title:                "ランダムな数値の生成"
html_title:           "Haskell: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何ですか？: ランダムな数字の生成とは、プログラマーがランダムまたは乱雑な値をプログラム内で使用するためにコンピュータ上で生成することです。プログラマーは、特定の範囲の値を必要とするデータや、シミュレーションやゲームなどのアプリケーションでランダムな振る舞いを作り出すために、ランダムな数字の生成を行います。

## 方法: ランダムな数字を生成するには、Haskellの組み込み関数である`random`を使用します。下のコードブロックでは、`random`関数を使用して0から10の範囲内のランダムな整数を生成し、その結果を出力します。

```Haskell
import System.Random

main = do
  randomNumber <- randomRIO(0, 10) -- `randomRIO`は0から10の範囲内でランダムな整数を生成する関数です。
  print randomNumber -- `print`関数は値をコンソールに出力するための関数です。
```

上記のコードを実行すると、毎回異なる値が出力されることがわかります。

## ディープダイブ: ランダムな数字の生成は、コンピュータの内部的な状態を使用して行われます。この状態は乱数生成器と呼ばれ、ランダム性の根本的な基準となります。Haskellでは、様々な乱数生成器が利用可能であり、それぞれ異なるランダム性の品質を持っています。また、Haskellには`random`以外にも`randomIO`や`randoms`といった関数があり、それぞれ異なる方法でランダムな値を生成します。

## 関連リンク: 
- Haskell公式ドキュメントのランダムモジュールのページ: https://hackage.haskell.org/package/random/docs/System-Random.html
- Haskellの乱数生成についての詳しい説明: http://learnyouahaskell.com/input-and-output#randomness