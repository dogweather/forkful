---
aliases:
- /ja/haskell/handling-errors/
date: 2024-01-26 00:54:00.007412-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u30A8\
  \u30E9\u30FC\u51E6\u7406\u306F\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\u2015\u554F\
  \u984C\u304C\u751F\u3058\u3046\u308B\u3053\u3068\u2015\u3092\u7BA1\u7406\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\
  \u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3053\
  \u308C\u3089\u306E\u72B6\u6CC1\u306B\u5BFE\u3057\u3066\u512A\u96C5\u306B\u5BFE\u51E6\
  \u3067\u304D\u308B\u3088\u3046\u306B\u3057\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3057\
  \u305F\u308A\u9593\u9055\u3063\u305F\u7D50\u679C\u3092\u751F\u6210\u3057\u305F\u308A\
  \u3057\u306A\u3044\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.963672
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u30A8\
  \u30E9\u30FC\u51E6\u7406\u306F\u4E88\u671F\u3057\u306A\u3044\u4E8B\u614B\u2015\u554F\
  \u984C\u304C\u751F\u3058\u3046\u308B\u3053\u3068\u2015\u3092\u7BA1\u7406\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\
  \u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3053\
  \u308C\u3089\u306E\u72B6\u6CC1\u306B\u5BFE\u3057\u3066\u512A\u96C5\u306B\u5BFE\u51E6\
  \u3067\u304D\u308B\u3088\u3046\u306B\u3057\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3057\
  \u305F\u308A\u9593\u9055\u3063\u305F\u7D50\u679C\u3092\u751F\u6210\u3057\u305F\u308A\
  \u3057\u306A\u3044\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおけるエラー処理は予期しない事態―問題が生じうること―を管理することです。プログラマーはそれを行うことで、プログラムがこれらの状況に対して優雅に対処できるようにし、クラッシュしたり間違った結果を生成したりしないようにします。

## 方法：
Haskellは`Maybe`や`Either`のような型を使って頑健にエラーを処理します。こちらが簡単な例です：

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- ゼロで割ることはできないので、Nothingを返します。
safeDivide x y = Just (x `div` y)  -- それ以外の場合は問題ないので、結果をJustに入れて返します。

-- 動作例を見てみましょう：
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

より複雑なエラー処理のためには、`Either`が登場します：

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- 今回はエラーにメッセージが添えられます。
safeDivideEither x y = Right (x `div` y)

-- 使用例：
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## 深掘り
Haskellの世界では、エラー処理は長い歴史を持っています。昔はエラーがプログラム全体を停止させてしまうこともありました―楽しくありません。Haskellの型システムは、これをはるかに起こりにくくする方法を提供します。`Maybe`や`Either`がありますが、`Exceptions`や`IO`などの異なる状況に応じた他のものもあります。

`Maybe`はシンプルです：すべてが上手く行っていれば`Just`何かが得られ、そうでなければ`Nothing`です。`Either`は一歩進んで、エラーメッセージ（`Left`）または成功した結果（`Right`）を返すことができます。

どちらも純粋で、つまり外の世界に干渉しないというのがHaskellにとって大きな取引です。私たちは、いくつかの他の言語に問題を起こしている未チェックの例外の落とし穴を避けます。

`Maybe`や`Either`だけで満足できない人のために、`Control.Exception`のようなライブラリが、例外を通じたより伝統的な手続き型のエラー処理を提供します。しかし、それらを過剰に使うことは複雑化を招くことがあるので、コミュニティはしばしば型に固執します。

## 参照
さらに深く掘り下げるには：

- Haskell自身のドキュメント： [Haskell](https://haskell.org/documentation)
- 初心者に最適： ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
