---
title:    "Haskell: 一時ファイルの作成"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ作るのか

一時的なファイルを作成する理由は何でしょうか？これはプログラマーにとって非常に重要な質問です。一時的なファイルを作成すると、一時的なデータを保存することができ、プログラムのメモリ使用量を減らすことができます。また、一時的なファイルを作成することで、プログラムをより効率的に実行することができます。

## 作り方

Haskellを使用して一時的なファイルを作成する方法はいくつかあります。最も一般的な方法は、`createOrOpenTempFile`関数を使用する方法です。

```Haskell
import System.IO

main = do
  (tempName, tempHandle) <- createOrOpenTempFile "." "temp.txt"
  putStrLn $ "一時的なファイルを作成しました。ファイル名は " ++ tempName ++ " です。"
  hPutStrLn tempHandle "これは一時的なファイルです。"
  hClose tempHandle
```

上記のコードを実行すると、カレントディレクトリに`temp.txt`という名前の一時的なファイルが作成されます。また、ファイルには`These are temporary files.`というテキストが書き込まれます。`createOrOpenTempFile`関数以外にも、`withSystemTempFile`や`withTempFile`などの関数を使用する方法もあります。

## 深堀り

一時的なファイルを作成する際には、ファイルパスを生成するために、システムの一時ディレクトリやカレントディレクトリを使用します。また、作成された一時的なファイルはプログラムが終了すると自動的に削除されます。これにより、プログラムが使用したリソースをクリーンアップすることができます。

## 他にも参考になる記事

- [Haskell ドキュメント - createOrOpenTempFile関数](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html#g:3)
- [Real World Haskell - 学習リソース](http://book.realworldhaskell.org/read/resources.html)
- [AtCoder - Haskell 導入編](https://qiita.com/sylph_scene/items/bb89a4decb623fa42c32)