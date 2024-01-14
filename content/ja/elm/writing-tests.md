---
title:    "Elm: テストの書き方"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストを書くことは、エラーやバグを事前に発見することができるため、コードの品質を向上させるために非常に重要です。また、テストを書くことで、コードの変更や改善を行った際に、予期しない影響が起きていないかを確認することができます。

## テストの書き方

テストを書くためには、まずはElmコードをテスト可能なように分割する必要があります。具体的なコード例を示します。

```Elm
-- オリジナルのコード
add : Int -> Int -> Int
add x y =
  x + y

-- テスト可能なコードに分割
add : Int -> Int -> Int
add x y =
  x + y

addTest : List (Int, Int, Int)
addTest =
  [ (1, 2, 3)
  , (5, 6, 11)
  , (10, 5, 15)
  ]

testAdd : Bool
testAdd =
  List.all (\(x, y, expected) -> add x y == expected) addTest
```

テスト可能なコードに分割することで、入力と期待する出力を対応させることができ、複数のテストケースをまとめて実行することも可能になります。

## 奥深くテストを書く

テストを書く際には、以下のようなポイントに注意することで、より効果的なテストを作成することができます。

- テスト対象の関数やコードの特定の条件下での動作を確認するテストを書く
- テストする値や入力をランダムに生成することで、より広範囲なケースをカバーするようにする
- テストケースを他のエンジニアと共有し、フィードバックを受けることで、抜け漏れや改善点を発見する

これらのポイントを意識することで、よりロバストかつ有用なテストを作ることができます。

## 参考リンク

[Elm公式ガイド: テスト](https://guide.elm-lang.org/testing/)

[Elm公式ドキュメント: fuzz](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#fuzz)

See Also:

参考リンク:
- [Elm公式ガイド: テスト](https://guide.elm-lang.org/testing/)
- [Elm公式ドキュメント: fuzz](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#fuzz)