---
title:    "Haskell: ディレクトリが存在するかどうかを確認する"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

＃＃＃なぜ
ディレクトリの存在を確認する必要があるのかを説明します。

ディレクトリが存在するかどうかをチェックする理由はさまざまです。たとえば、プログラムが特定のディレクトリ内のファイルを操作する必要がある場合、そのディレクトリが存在しない場合には、エラーを防ぐために事前に存在をチェックする必要があります。

＃＃＃ハウトゥ
ディレクトリの存在をチェックする方法を説明します。

```Haskell
import System.Directory

main = do
  -- チェックするディレクトリのパスを定義する
  let directory = "絶対パス/チェックするディレクトリ"
  -- exists関数を使ってディレクトリが存在するかどうかを確認する
  exists <- doesDirectoryExist directory
  -- ディレクトリが存在した場合には"存在します"と出力する
  if exists
    then putStrLn "存在します"
    -- ディレクトリが存在しなかった場合には"存在しません"と出力する
    else putStrLn "存在しません"
```

上記のコードでは、System.DirectoryモジュールのdoesDirectoryExist関数を使用して、指定したディレクトリが存在するかどうかを確認しています。ディレクトリが存在する場合にはTrue、存在しない場合にはFalseを返します。また、結果に応じて適切なメッセージを出力するように設定しています。

＃＃＃ディープダイブ
ディレクトリの存在をチェックするためのさまざまな方法について、さらに詳しく説明します。

ハスケルでは、System.Directoryモジュールの他にもSystem.Posix.Filesモジュールを使用することで、ディレクトリの存在をチェックすることができます。また、例外処理を使用することでより詳細なエラーハンドリングを実装することも可能です。

＃＃＃参照
こちらのリンクを参考にしてください。  
- [Hackageドキュメント (System.Directory)](https://hackage.haskell.org/package/directory-1.3.4.1/docs/System-Directory.html)
- [Hackageドキュメント (System.Posix.Files)](https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Files.html)