---
title:                "Haskell: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ 
プログラミングをやり始める理由はさまざまですが、新しいプロジェクトを始めることでより深く学習することができます。Haskellは素晴らしい言語であり、新しいプロジェクトを始めることでその力を実感することができます。

## プロジェクトを始める方法
まずは、Haskellプロジェクトを始めるために必要なものをインストールしましょう。次に、お持ちのテキストエディタで新しいファイルを作成し、Haskellのコードを書き始めましょう。

```Haskell
-- Main.hsという新しいファイルを作成します。
-- ここにコードを書いていきます。
```

新しいプロジェクトを始める際には、最も基本的な「Hello World」プログラムを書くことから始めましょう。

```Haskell
-- Main.hsに以下のコードを書きます。
main = putStrLn "Hello World"

-- コンパイルして実行します。
-- ターミナルで以下のコマンドを実行します。
-- ghc Main.hs -o hello
-- ./hello と入力すると、"Hello World"と表示されます。
```

さらに、Haskellの強力な型システムを活用しながら、新しいプロジェクトを作成することができます。例えば、以下のようにリストの要素を足し合わせる関数を作成することができます。

```Haskell
-- Main.hsに以下のコードを書きます。
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

main = do
    let numbers = [1,2,3]
    putStrLn $ "The sum of " ++ show numbers ++ " is " ++ show (mySum numbers)

-- コンパイルして実行します。
-- ターミナルで以下のコマンドを実行します。
-- ghc Main.hs -o sum
-- ./sum と入力すると、"The sum of [1,2,3] is 6"と表示されます。
```

## ディープダイブ
新しいプロジェクトを始める際には、Haskellのモジュールシステムやパッケージマネージャーを活用することも重要です。また、HackageやGitHubなどのコミュニティリソースを活用しながら、より大きなプロジェクトを始めることも可能です。さらに、Haskellの関数型プログラミングのパラダイムや型クラスなど、興味深いトピックを学習することもできます。

## See Also
- [Haskell プログラミングガイド](https://www.stackage.org/haddock/guide.html)
- [Haskell School of Music](http://haskell.cs.yale.edu/?post_type=publication&p=112)
- [Real World Haskell](http://book.realworldhaskell.org/read/)
- [Hackage](https://hackage.haskell.org/) 
- [GitHub](https://github.com/)