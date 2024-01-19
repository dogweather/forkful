---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
プログラムでは、一時ファイルの作成がよく行われます。一時ファイルは、データを一時的に格納するためのファイルで、大量のデータ処理やファイル間でのデータ移行に有用です。

## 作り方：
Haskellで一時ファイルを作るためのライブラリには[System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)があります。このライブラリを使って簡単な一時ファイル作成のコードを以下に示します。

```Haskell
import System.IO.Temp

myFunc :: IO ()
myFunc = withSystemTempFile "my_temp.txt" $ \fp h -> do
            hPutStrLn h "Temporary Data"
            hClose h
            putStrLn $ "Created a temp file at " ++ fp
            -- fp: ファイルのパス, h: ファイルへのハンドル
```
この実行すると以下のような出力が得られます。

```Haskell
"Created a temp file at /tmp/my_temp.txt12345"
```

## ディープダイブ：
Haskellは一時ファイルの作成が可能な数多くの言語の一つです。この機能が追加されたのは、大規模なデータを処理しやすくするためです。また、ファイルを開く代わりにメモリーにデータをホールドするほうが便利なケースもあるため、Haskellには[`Data.ByteString`](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html)等の代替モジュールも提供されています。System.IO.Tempの内部では、`openTempFile`というHaskellの標準ライブラリ関数が使われています。

## 参考に：
- [Haskell System.IO.Temp Documentation](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Haskell Cookbook - Managing Temporary Files and Directories](https://haskell.fpcomplete.com/tutorial/managing-temporary-files-and-directories)
- [Haskell Tempfile Source Code](https://github.com/haskell/unix/blob/master/System/Posix/Temp.hsc)