---
title:                "Haskell: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時的なファイルを作成することについて、なぜ取り組むべきかを説明します。

一時的なファイルを作成する最も一般的な理由は、プログラムの実行中に一時的な情報を保存する必要がある場合です。例えば、大量のデータを処理するプログラムで一時ファイルを使用することで、メモリの使用量を減らすことができます。

## 作り方
まず、一時ファイルを扱うためのモジュールをインポートする必要があります。

```Haskell
import System.IO
import System.Directory
```

次に、`withTempFile`関数を使用して一時ファイルを作成します。

```Haskell
withTempFile :: FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
```

この関数は、3つの引数を取ります。`FilePath`は一時ファイルの保存先を指定します。`String`は一時ファイルの接頭辞となる文字列です。そして、最後の引数は一時ファイルが作成された後に実行される関数です。

例えば、次のようなコードを書くことで、一時ファイルを作成し、データを書き込み、読み込むことができます。

```Haskell
withTempFile "tmp/" "example" $ \fileName handle -> do
    hPutStrLn handle "Hello World"
    hGetContents handle
```

上記のコードでは、"tmp/"というディレクトリに"example"という接頭辞を持つ一時ファイルが作成されます。そして、`hPutStrLn`関数を使用してテキストを書き込み、`hGetContents`関数を使用してそのテキストを読み込んでいます。

## 深堀り
一時ファイルを作成する際には、ファイル名の重複に注意が必要です。`withTempFile`関数では、自動的にユニークなファイル名を生成して改名するため、ファイル名の重複を心配する必要はありません。

また、一時ファイルを作成する際には、後で削除する必要があるため、`withTempFile`関数の最後に`removeFile`関数を使用して一時ファイルを削除することをお勧めします。

## 参考リンク
- [Haskellの公式ドキュメント](https://downloads.haskell.org/~ghc/8.10.3/docs/html/libraries/base-4.14.1.0/System-IO.html#v:withTempFile)
- [Real World Haskellのチュートリアル](http://book.realworldhaskell.org/read/io.html#id773870)

## 参考
[Haskellで一時ファイルを作成する](https://example.com)