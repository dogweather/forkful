---
title:    "Haskell: 一時ファイルの作成"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成することに参加する理由は、一時的なデータの保管や処理のためです。

## 作り方

Haskellで一時ファイルを作成する最も簡単な方法は、`System.IO.tempFile`関数を使用することです。

```
import System.IO

main = do
  temp <- tempFile "tmp_files\\" "hello.txt"
  hPutStrLn temp "Hello world"
  hClose temp
```

このコードは、`tmp_files`ディレクトリ内に`hello.txt`という名前の一時ファイルを作成し、その中に「Hello world」というテキストを書き込みます。最後に、ファイルを閉じてリソースを解放します。

また、一時ファイルを扱うときには、`withTempFile`関数を使用することもできます。

```
import System.IO

main = withTempFile "tmp_files\\" "hello.txt" $ \temp -> do
  hPutStrLn temp "Hello world"
```

この方法では、`temp`変数が一時ファイルを指すようになります。今度は、`hClose`を呼ぶ必要はありません。`withTempFile`は、ファイルを自動的に閉じてくれます。

## 深堀り

一時ファイルを作成するときは、ファイルへのパスやハンドルを指定する必要があります。しかし、パスやハンドルを指定することができない場合はどうすればよいでしょうか？そのような場合には、`withSystemTempFile`関数を使用することができます。

```
import System.IO

main = withSystemTempFile "hello.txt" $ \temp -> do
  hPutStrLn temp "Hello world"
```

このコードでは、一時ファイルを作成するためのディレクトリや接頭辞を指定する必要がありません。Haskellが自動的に適切な場所を選んでくれます。

一時ファイルを作成した後は、必ずファイルを閉じてリソースを解放するようにしましょう。それには、`hClose`関数を使用します。

## 参考リンク

- [Hackage: System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [TempFile Library for Haskell](https://github.com/ryantrinkle/tempfile)
- [Creating Temporary Files in Haskell](https://riptutorial.com/haskell/example/2497/creating-temporary-files-in-haskell)

## 参照先