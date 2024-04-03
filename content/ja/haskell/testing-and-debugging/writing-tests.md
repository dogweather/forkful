---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:53.298196-07:00
description: "\u65B9\u6CD5:\u2026"
lastmod: '2024-03-13T22:44:42.189150-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u306F\u3055\u307E\u3056\u307E\u306A\u30C6\u30B9\u30C8\u30D5\u30EC\
  \u30FC\u30E0\u30EF\u30FC\u30AF\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\
  \u3059\u304C\u3001`Hspec`\u3068`QuickCheck`\u306E\u4E8C\u3064\u304C\u4EBA\u6C17\u3067\
  \u3059\u3002Hspec\u3067\u306F\u3001\u30B3\u30FC\u30C9\u306E\u305F\u3081\u306E\u4EBA\
  \u9593\u304C\u8AAD\u3081\u308B\u4ED5\u69D8\u3092\u5B9A\u7FA9\u3067\u304D\u308B\u4E00\
  \u65B9\u3067\u3001QuickCheck\u3067\u306F\u3001\u30B3\u30FC\u30C9\u304C\u6E80\u305F\
  \u3059\u3079\u304D\u6027\u8CEA\u3092\u8AAC\u660E\u3059\u308B\u3053\u3068\u306B\u3088\
  \u308A\u30C6\u30B9\u30C8\u3092\u81EA\u52D5\u7684\u306B\u751F\u6210\u3055\u305B\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
