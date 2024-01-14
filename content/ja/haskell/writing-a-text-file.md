---
title:    "Haskell: テキストファイルを作成する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書くことに参加する理由は、Haskellプログラミングの基本的なスキルを学ぶためです。テキストファイルを書くことは、データのストレージや情報の共有に非常に重要です。また、Haskellプログラミングによって、コードの再利用性や読みやすさが向上します。

## ハウツー

テキストファイルを書くための基本的な手順は以下の通りです。

1. `haskell`のモジュールをインポートする。
2. テキストファイルのパスを指定する。
3. テキストファイルを開くための関数`openFile`を使用する。
4. `hPutStr`関数を使用して、ファイルに書き込むデータを指定する。
5. ファイルを閉じるための`hClose`関数を使用する。

以下のコードは、上記の手順を実装した例です。

```Haskell
import System.IO

main = do
  let path = "sample.txt"
  file <- openFile path WriteMode
  hPutStr file "This is a sample text file."
  hClose file
```

上記のコードを実行すると、`sample.txt`というファイルが作成され、その中に`This is a sample text file.`というテキストが書き込まれます。

## ディープダイブ

テキストファイルを書くためには、さまざまなオプションや関数があります。ファイルを開く際には、読み書きのモードや文字コードを指定することができます。また、`hPutStrLn`関数を使用すれば、改行付きのテキストを書き込むこともできます。

テキストファイルを上書きするのではなく、追記する場合は`AppendMode`を使用します。さらに、`withFile`関数を使用することで、ファイルを自動的に閉じることができます。

テキストファイルの書き方に関するさらなる詳細は、公式のHaskellドキュメントを参照してください。

## その他のリンク

- [Haskell公式ドキュメント](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskellでテキストファイルを読み書きする方法](https://qiita.com/sasaplus1/items/300fc9a819055afd2fa6)