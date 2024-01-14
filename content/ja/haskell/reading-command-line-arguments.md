---
title:    "Haskell: コンピュータ・プログラミングにおける「コマンドライン引数の読み取り」"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

こんにちは、Haskellプログラミングブログへようこそ！今日は、コマンドライン引数を読み取る方法についてお話しします。コマンドライン引数を読み取ることで、プログラムをより柔軟に動かすことができます。さっそく、始めましょう！

## なぜ

コマンドライン引数を読み取ることの利点は、プログラムを実行する際に様々なパラメーターを指定できることです。例えば、ファイル名やオプションの値などを指定することができます。これにより、プログラムを実行する際に都合の良い設定を行うことができます。

## 方法

では、実際にコマンドライン引数を読み取る方法を見てみましょう。下のコードを参考にしてください。

```Haskell
import System.Environment

main = do
  args <- getArgs
  print args
```

このコードは、`System.Environment`モジュールをインポートし、`getArgs`関数を使って引数を取得し、それを表示します。コマンドラインで`runhaskell arguments.hs hello world`と実行すると、`["hello", "world"]`という出力が得られます。

## さらに深く

コマンドライン引数を取得する方法のほかにも、さらに高度な使い方があります。例えば、オプション引数を扱うためのオプションパーサーを作ることや、引数の妥当性をチェックすることができます。詳しい情報は、[Haskellの公式ドキュメント](https://www.haskell.org/documentation/)や[Real World Haskell](http://book.realworldhaskell.org/)などを参考にしてください。

## 関連リンク

- [Haskell公式ドキュメント](https://www.haskell.org/documentation/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Wiki](https://wiki.haskell.org/)
- [Haskell Reddit](https://www.reddit.com/r/haskell/)