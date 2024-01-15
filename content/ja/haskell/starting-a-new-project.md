---
title:                "新しいプロジェクトを開始する"
html_title:           "Haskell: 新しいプロジェクトを開始する"
simple_title:         "新しいプロジェクトを開始する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

プロジェクトを立ち上げる理由はたくさんあります。新しいアイデアを実現したい、スキルを磨きたい、または単に挑戦を楽しみたいというのもその一つです。Haskellは関数型言語として優れたパフォーマンスを発揮し、複雑な問題を解決するのに向いています。新しいプロジェクトでHaskellを使うことで、多くのメリットが得られるでしょう。

## How To

プロジェクトを始めるには、まず必要なものを揃える必要があります。Haskellの開発環境をインストールすることが第一歩です。公式のGHCプラットフォームやStackを使うことがおすすめです。

プロジェクトを立ち上げるためには、まずはソースコードを作成する必要があります。プロジェクトのディレクトリ内に、 `main.hs` という名前のファイルを作成しましょう。そして次のようなコードを書き込んでください。

```Haskell
import Data.Char

main = do
  -- 入力を受け取る
  putStrLn "名前を入力してください: "
  name <- getLine

  -- 入力された名前を大文字に変換して出力する
  putStrLn $ "あなたの名前は " ++ map toUpper name ++ " です。"
```

これで準備は完了です。次に、プロジェクトのディレクトリ内で以下のコマンドを実行してください。

```
stack runghc main.hs
```

すると、コンソール上に「名前を入力してください」というメッセージが表示されます。適当な名前を入力した後にエンターキーを押すと、入力された名前が大文字に変換されて表示されるはずです。

## Deep Dive

新しいプロジェクトを始める際には、必ず環境構築やコンパイルの方法などの基本的な部分から始める必要があります。また、プロジェクトが複雑になってきた場合には、様々なライブラリを使うことで効率的に開発を進めることができます。よく使われるライブラリとしては、パーサーやWebフレームワークなどがあります。

また、HaskellにはユニットテストのためのフレームワークとしてHspecがあります。これを使うことで、コードの品質を保証しながら開発を進めることができます。

プロジェクトを始める際には、これらの情報を参考にして効率的に開発を進めるよう心がけましょう。

## See Also

- [Official GHC Platform](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Hspec](https://hspec.github.io/)
- [Haskellパーサーの例](http://book.realworldhaskell.org/read/using-parsec.html)