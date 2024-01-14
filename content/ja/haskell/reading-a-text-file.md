---
title:                "Haskell: テキストファイルの読み込み"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ホワイ

テキストファイルを読み込むことは、プログラミングにおいて非常に重要です。そのため、Haskellを学ぶ上で基本的なスキルとなります。このブログ投稿では、Haskellでテキストファイルを読み込む方法について紹介します。

## ハウトゥ

まずは、テキストファイルを読み込むための基本的なコードを見ていきましょう。

```Haskell
import System.IO

main = do
    handle <- openFile "sample.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
```

まず、`System.IO`モジュールをインポートします。その後、`openFile`関数を使って、`sample.txt`という名前のファイルを読み込みモードで開きます。`hGetContents`関数はハンドルからテキストを読み込みます。そして、`putStrLn`関数を使って、読み込んだ内容をターミナルに出力します。最後に、`hClose`関数でハンドルを閉じて処理を終了します。

## ディープダイブ

テキストファイルを読み込む際に重要なのは、ファイルのエンコーディングについてです。Haskellの`hGetContents`関数は、デフォルトではUTF-8エンコーディングを使用してテキストを読み込みます。しかし、ファイルのエンコーディングが異なる場合は、`hSetEncoding`関数を使って明示的に指定する必要があります。

また、`hGetContents`関数は、ファイルを全て一度に読み込むのではなく、必要に応じて遅延評価を行います。これにより、大きなファイルをメモリに読み込まずに処理することができます。

さらに、テキストファイルを読み込む際に利用できる便利な関数やモジュールもあります。例えば、`readFile`関数や`Text.Encoding`モジュールなどが挙げられます。これらを活用することで、より柔軟なテキスト処理を行うことができます。

## シーアルソーシリンク

- [Haskellでテキストファイルを書き込む方法](https://www.haskell.org/tutorial/io.html#reading-and-writing-files)
- [HaskellのIOモナドについて](https://wiki.haskell.org/IO_inside)
- [Haskellでのファイル取得のさまざまな方法](https://lgo-tera.hatenablog.com/entry/20171120/1511172656)

## 関連リンク

- [Haskell.jp](https://haskell.jp/)
- [Haskellの基本的な文法](https://qiita.com/7shi/items/145f1232a2c4ad3f9b96)
- [Haskellのコードを読めるようになろう](https://qiita.com/tanakh/items/0ba42c7ca36cd29d0ac8)