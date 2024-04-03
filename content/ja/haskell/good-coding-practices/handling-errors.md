---
date: 2024-01-26 00:54:00.007412-07:00
description: "\u65B9\u6CD5\uFF1A Haskell\u306F`Maybe`\u3084`Either`\u306E\u3088\u3046\
  \u306A\u578B\u3092\u4F7F\u3063\u3066\u9811\u5065\u306B\u30A8\u30E9\u30FC\u3092\u51E6\
  \u7406\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.195183-06:00'
model: gpt-4-1106-preview
summary: "Haskell\u306F`Maybe`\u3084`Either`\u306E\u3088\u3046\u306A\u578B\u3092\u4F7F\
  \u3063\u3066\u9811\u5065\u306B\u30A8\u30E9\u30FC\u3092\u51E6\u7406\u3057\u307E\u3059\
  \u3002\u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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
