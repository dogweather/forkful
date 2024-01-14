---
title:    "Haskell: テキストファイルの読み込み"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読むことの利点は、コンピューターから情報を取得する最も基本的で効率的な方法です。また、テキストファイルは読みやすく、他のプログラミング言語にも簡単に移植できるため、Haskellプログラマーにとって非常に便利です。

## 方法

以下のコードブロックには、Haskellでテキストファイルを読み取る方法の例が記載されています。コードの下には、実際の出力が表示されます。

```Haskell
import System.IO

main = do
    handle <- openFile "sample.txt" ReadMode  -- ファイルを開く
    contents <- hGetContents handle  -- ファイルの内容を取得する
    putStr contents  -- 内容を表示する
    hClose handle  -- ファイルを閉じる
```

実際の出力:

```
This is a sample text file.
It contains some sample text.
```

## 深堀り

テキストファイルを読み取る際に、いくつかの注意点があります。まず、ファイルを開く際には`openFile`関数を使用し、第二引数には読み取りモードを指定する必要があります。また、ファイルを閉じる際には`hClose`関数を使用することで、プログラムの最後にファイルが閉じられないように注意する必要があります。

## 参考リンク

- [Haskell: テキストファイルを読み込む方法](https://techacademy.jp/magazine/20893)
- [Haskellの典型的なIOプログラム](https://qiita.com/nwtgck/items/217e3896709e0a683cbc)
- [Haskellのファイル入出力について](https://qiita.com/yamamotoj/items/9c0b234737180b64ab97)

## 関連リンク

- [Markdownとは？メリットや書き方の基礎を解説](https://www.sejuku.net/blog/26114)
- [Markdown記法チートシート](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)
- [Markdownの使い方～基本文法～](https://qiita.com/tbpgr/items/989c6badefff69377da7)