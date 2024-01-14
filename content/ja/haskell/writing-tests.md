---
title:                "Haskell: テストを書く"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ書くのか

プログラミングをする上で、私たちは常にコードの品質を重視します。コードが正しく動作することを確認するために、テストが欠かせません。テストを書くことによって、コードのバグを早期に発見し、修正することができるようになります。

## どのように書くのか

テストを書くためには、まずテストフレームワークをインポートする必要があります。以下のコードは、HUnitというテストフレームワークを使用した、単純な数値の比較を行うテストの例です。
```Haskell
module Main where

import Test.HUnit

-- テストする関数
add :: Int -> Int -> Int
add x y = x + y

-- テストケース
testAdd :: Test
testAdd = TestCase (assertEqual "1 + 1 は 2 に等しい" 2 (add 1 1))

-- テストスイート
tests :: Test
tests = TestList [TestLabel "testAdd" testAdd]

-- メイン関数
main :: IO ()
main = runTestTT tests
```

上記のコードを実行すると、以下のような結果が得られます。

```
Cases: 1 Tried: 1 Errors: 0 Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

このように、テストが全て通過したことが表示され、プログラムの正しさが確認できます。

## 深く掘り下げる

テストを書く際には、テストケースをできるだけ網羅的に設計することが重要です。また、コードの特定の部分だけでなく、複数の関数を組み合わせてテストすることも大切です。さらに、コードのバグを発見した際には、そのバグを再現するテストケースも書くことが重要です。

## 参考リンク

- [HUnit公式ドキュメント](https://hackage.haskell.org/package/HUnit)
- [テスト駆動開発の基本](https://codezine.jp/article/detail/565)
- [Haskellのテストフレームワーク一覧](https://qiita.com/nwtgck/items/29d1b1accb1e3e5d55f6)