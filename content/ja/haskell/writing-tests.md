---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストを書くこととその理由)
テストを書くとは、コードの正しさを自動で検証する手段です。これにより、バグを減らし、コード改良時の信頼性を高めることができます。

## How to: (やり方)
Haskellでのテストは`Hspec`ライブラリが一般的。下記は`stack test`で実行可能なサンプルコード。

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "length" $ do
    it "returns the number of elements in a list" $ do
      length [1, 2, 3] `shouldBe` 3

-- サンプル出力
-- length
--   returns the number of elements in a list
```

## Deep Dive (深掘り)
Haskellのテストは、QuickCheckのようなプロパティベーステストツールから始まりました。HspecはBDDにインスピレーションを受けたライブラリです。`hspec-discover`はテストを自動検出する機能を提供。代替として、`doctest`や`Tasty`がありますが、Hspecが一番普及しています。

## See Also (参考情報)
- Hspec公式ドキュメント: [http://hspec.github.io/](http://hspec.github.io/)
- QuickCheck: [https://hackage.haskell.org/package/QuickCheck](https://hackage.haskell.org/package/QuickCheck)
- Tasty: [https://hackage.haskell.org/package/tasty](https://hackage.haskell.org/package/tasty)
- Doctest: [https://hackage.haskell.org/package/doctest](https://hackage.haskell.org/package/doctest)
