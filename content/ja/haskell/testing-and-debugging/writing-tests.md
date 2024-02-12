---
title:                "テストの作成"
date:                  2024-02-03T19:30:53.298196-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Haskellでテストを書くことは、自動化されたチェックを通じて関数が期待どおりに機能することを確認することについてです。プログラマーは、早期にバグを捕捉し、リファクタリングを容易にし、振る舞いを文書化することで、コードベースをより保守しやすく、スケーラブルにするためにこれを行います。

## 方法:

Haskellはさまざまなテストフレームワークをサポートしていますが、`Hspec`と`QuickCheck`の二つが人気です。Hspecでは、コードのための人間が読める仕様を定義できる一方で、QuickCheckでは、コードが満たすべき性質を説明することによりテストを自動的に生成させることができます。

### Hspecの使用

まず、ビルドツール設定（例えば、`stack.yaml`や`cabal`ファイル）に`hspec`を追加します。そして、`Test.Hspec`をインポートし、仕様としてテストを書きます：

```haskell
-- file: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "二つの数値を加算する" $
    add 1 2 `shouldBe` 3

  it "ゼロを加算した場合は、最初の数値を返す" $
    add 5 0 `shouldBe` 5
```

次に、ビルドツールを使ってテストを実行し、次のような出力が得られるかもしれません：

```
MyLib.add
  - 二つの数値を加算する
  - ゼロを加算した場合は、最初の数値を返す

0.0001秒で終了
2例、0失敗
```

### QuickCheckの使用

QuickCheckでは、関数が満たすべき性質を表現します。プロジェクト設定に`QuickCheck`を追加し、それをインポートします：

```haskell
-- file: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

これらのテストを実行すると、指定された性質をチェックするための入力が自動生成されます：

```
+++ OK, 100回のテストをパスしました。
+++ OK, 100回のテストをパスしました。
```

HspecとQuickCheckの両方の例では、テストスイートは実行可能な文書として機能し、コードの正確さを自動的に検証できます。
