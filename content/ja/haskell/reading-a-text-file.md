---
title:    "Haskell: テキストファイルを読む"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、Haskellプログラミングで非常に役立ちます。例えば、大量のデータを処理する際に読み込むことで、簡単に情報を取得することができます。また、テキストファイルを読み込むと、ファイル内のデータを変更することができるため、柔軟性の高いプログラミングが可能になります。

## 方法

Haskellでテキストファイルを読み込むには、コードブロック内に ```Haskell ... ``` を使用します。以下に例を示します。

```Haskell
main = do
  -- ファイルを読み込む
  file <- readFile "sample.txt"

  -- ファイルの内容を出力する
  putStr file
```

上記のコードでは、```sample.txt```というファイルを読み込み、その内容を出力しています。これで実行すると、ターミナルにファイルの内容が表示されるでしょう。

## 深堀り

ここでは、より深くテキストファイルを読み込む方法について説明します。まずは、ファイルの内容を1行ずつ読み込む方法です。

```Haskell
main = do
  -- ファイルを読み込む
  file <- readFile "sample.txt"

  -- ファイルの内容を1行ずつ表示する
  let linesOfFile = lines file
  mapM_ putStrLn linesOfFile
```

上記のコードでは、```lines```関数を使用してファイルの内容を1行ずつ取得し、```putStrLn```関数を使用して出力しています。

次に、特定の文字列を含む行のみを取得する方法です。

```Haskell
main = do
  -- ファイルを読み込む
  file <- readFile "sample.txt"

  -- 特定の文字列を含む行のみを抽出する
  let filteredLines = filter ("haskell" `isInfixOf`) (lines file)
  mapM_ putStrLn filteredLines
```

上記のコードでは、```filter```関数を使用して、「haskell」という文字列を含む行のみを抽出し、```putStrLn```関数を使用して出力しています。

## 参考リンク

- [Haskellでテキストファイルを扱う方法](https://www.haskell.org/haskellwiki/Handling_text_files)
- [Haskellの基本的な文法](https://qiita.com/but-honestly/items/100e48aac1ff4a778445)
- [Haskellでのリスト操作](https://qiita.com/satosystems/items/107dd6eaf63032fa0ddb)
- [HaskellでのIO操作](https://qiita.com/CHT4U/items/1e22d0a51aa59504a936)

## ご参考

- [Haskell Markdownの使い方](https://qiita.com/naoya@github/items/6d4931b14c4a65d61f7c)