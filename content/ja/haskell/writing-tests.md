---
title:                "Haskell: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングをする際、テストを書くことは非常に重要です。テストを書くことで、コードにバグやエラーがないかを確認することができます。また、将来コードを変更する際にも、テストを使うことで予期せぬバグを防ぐことができます。

## テストの書き方

テストを書くために、まずはHaskellのテストフレームワークであるHUnitをインポートします。それから、次のようにテストケースを作成します。

```Haskell
import Test.HUnit
test1 = TestCase (assertEqual "1 + 1は2に等しい" 2 (1 + 1))
tests = TestList [TestLabel "test1" test1]
```

上記の例では、1 + 1が2に等しいかをテストしています。assertEqual関数を使うことで、実際の値と期待する値が等しいかを確認することができます。また、TestList関数を使うことで複数のテストケースを結合することができます。

最後に、main関数を使ってテストを実行します。

```Haskell
main = do
  runTestTT tests
```

実行結果は次のようになります。

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases=1, tried=1, errors=0, failures=0}
```

テストが1回実行され、エラーや失敗がないことが確認できます。

## 深堀り

テストを書く際のポイントとしては、実際の値と期待する値をよく考えることが重要です。また、異なるケースやエラー処理を考慮することも忘れずに行いましょう。さらに、多くのテストケースを書くことで、より確実なコードを書くことができます。

## また見てね

- [HUnitドキュメント](https://hackage.haskell.org/package/HUnit)
- [テスト駆動開発 (TDD) について](https://ja.wikipedia.org/wiki/%E3%83%86%E3%82%B9%E3%83%88%E9%A7%86%E5%8B%95%E9%96%8B%E7%99%BA)
- [Haskellでテスト駆動開発をする](https://qiita.com/stkdev/items/ff5cde445656f464dfb8)