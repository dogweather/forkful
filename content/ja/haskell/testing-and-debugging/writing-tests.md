---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:53.298196-07:00
description: "Haskell\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u81EA\u52D5\u5316\u3055\u308C\u305F\u30C1\u30A7\u30C3\u30AF\u3092\u901A\u3058\u3066\
  \u95A2\u6570\u304C\u671F\u5F85\u3069\u304A\u308A\u306B\u6A5F\u80FD\u3059\u308B\u3053\
  \u3068\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u65E9\u671F\u306B\u30D0\u30B0\
  \u3092\u6355\u6349\u3057\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\
  \u5BB9\u6613\u306B\u3057\u3001\u632F\u308B\u821E\u3044\u3092\u6587\u66F8\u5316\u3059\
  \u308B\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u3092\u3088\u308A\
  \u4FDD\u5B88\u3057\u3084\u3059\u304F\u3001\u30B9\u30B1\u30FC\u30E9\u30D6\u30EB\u306B\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.189150-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u81EA\u52D5\u5316\u3055\u308C\u305F\u30C1\u30A7\u30C3\u30AF\u3092\u901A\u3058\u3066\
  \u95A2\u6570\u304C\u671F\u5F85\u3069\u304A\u308A\u306B\u6A5F\u80FD\u3059\u308B\u3053\
  \u3068\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u65E9\u671F\u306B\u30D0\u30B0\
  \u3092\u6355\u6349\u3057\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\
  \u5BB9\u6613\u306B\u3057\u3001\u632F\u308B\u821E\u3044\u3092\u6587\u66F8\u5316\u3059\
  \u308B\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u3092\u3088\u308A\
  \u4FDD\u5B88\u3057\u3084\u3059\u304F\u3001\u30B9\u30B1\u30FC\u30E9\u30D6\u30EB\u306B\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
