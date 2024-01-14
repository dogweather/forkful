---
title:    "Haskell: 文字の下部を抽出する。"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

##「なぜ」
文字列から部分文字列を取り出すことの楽しさや便利さについて説明します。

##「やり方」
文字列から部分文字列を取り出す方法について、Haskellのコード例とサンプル出力を使用して説明します。まずは文字列の最初の3文字を取り出す例を見てみましょう。

```Haskell
substr :: String -> String
substr str = take 3 str 
```

入力した文字列が「Hello World」だった場合、出力は「Hel」となります。次に、文字列の3文字目から6文字目を取り出す例を見てみましょう。

```Haskell
substr :: String -> String 
substr str = take 6 (drop 3 str)
```

入力した文字列が「Hello World」だった場合、出力は「lo Wo」となります。また、部分文字列を調べる際には、`isInfixOf`関数を使用することで簡単に確認することができます。例えば、文字列「abc」が「abcdefg」の一部であるかを調べるコードは以下の通りです。

```Haskell
substr :: Bool 
substr = "abc" `isInfixOf` "abcdefg"
```

このコードでは出力は`True`となります。

##「ディープダイブ」
部分文字列を取り出す際には、`take`や`drop`、`isInfixOf`といったよく使われる関数以外にも、`subsequences`や`tails`といった関数が便利です。これらの関数を組み合わせて使用することで、さまざまなパターンで部分文字列を取り出すことができます。また、部分文字列だけではなく、パターンマッチングを使用して特定の単語を抽出することもできます。

##「参考リンク」
- [Haskellで文字列から部分文字列を取り出す方法](https://qiita.com/youneverdie/items/6dfc477aae234d216d3a)
- [Haskellの関数一覧](https://en.wikibooks.org/wiki/Haskell/Reference/Functions)
- [Haskellでのパターンマッチングの使用例](https://riptutorial.com/haskell/example/13522/pattern-matching)