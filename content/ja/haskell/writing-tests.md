---
title:                "テスト作成"
html_title:           "Haskell: テスト作成"
simple_title:         "テスト作成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

テストを書くとは、コードを実際に実行して、その動作が思った通りに動くかどうかを確認することです。プログラマーは、コードの品質を保証し、バグを見つけるために、テストを書きます。

## 方法：

以下のコードブロック内にある「Haskell ...」コード例とサンプル出力を使って、テストを書く方法を紹介します。

```Haskell
-- 例：
-- 定義された関数sumのテストを書く
sumTest = [sum [] == 0,
           sum [1,2,3] == 6,
           sum [-1, -2, -3] == -6]
           
-- 定義された関数gcdのテストを書く
gcdTest = [gcd 12 18 == 6,
           gcd 0 5 == 5,
           gcd 15 15 == 15]
           
-- GHCiで実行して確認する
*Main> sumTest
True
*Main> gcdTest
True
```

## 詳細を深める：

テストを書くことは、コードを品質保証するだけでなく、バグを早期に発見することも可能にします。この手法は、ソフトウェア開発において重要なプラクティスとなっています。Haskellでは、HUnitやQuickCheckといったフレームワークを使ってテストを書くことができます。

また、テスト駆動開発という手法では、まずテストを書いてからコードを実装することで、コードをより確実に動作させることができます。

## 関連情報を見る：

テストを書くことは、確実なコードを書くために不可欠です。Haskellには、さまざまなテストフレームワークやテスト駆動開発についての情報があります。以下のリンクを参考にしてください。

- [HUnit公式ドキュメント](https://hackage.haskell.org/package/HUnit)
- [QuickCheck公式ドキュメント](https://hackage.haskell.org/package/QuickCheck)
- [テスト駆動開発の概要](https://qiita.com/jflaw/items/524a162056a381925b67)