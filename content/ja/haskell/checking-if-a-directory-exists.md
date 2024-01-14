---
title:    "Haskell: ディレクトリが存在するかどうかを確認する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することについて学ぶのは、プログラムをより柔軟にするためです。

## 方法

ディレクトリが存在するかどうかを確認するには、Haskellのソースコード内に以下のコードを追加します。

```Haskell
import System.Directory

main = do
    dirExists <- doesDirectoryExist "myDirectory"
    if dirExists
        then putStrLn "myDirectory exists!"
        else putStrLn "myDirectory does not exist."
```

上記のコードでは、System.DirectoryモジュールからdoesDirectoryExist関数を使って、指定したディレクトリが存在するかどうかを確認しています。存在すればTrue、存在しなければFalseを返します。そしてif文を使って、その結果に応じてメッセージを出力しています。これで指定したディレクトリが存在するかどうかを簡単に確認することができます。

## 深層ダイブ

もし、指定したディレクトリが存在しない場合、どのようにエラーをハンドリングするかについて考えることができます。System.Directoryモジュールには、ディレクトリが存在しない場合のエラーメッセージをカスタマイズできる関数もあります。

また、他のプログラミング言語でも同じようにディレクトリが存在するかどうかを確認する方法がありますが、Haskellのように型安全な言語を使うことで、より信頼性の高いプログラムを作りやすくなります。

## 関連情報

* [HaskellのSystem.Directoryモジュールのドキュメント](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
* [ディレクトリを扱う上で覚えておきたいHaskellの基礎知識](https://qiita.com/yuroyoro/items/4effdbf7a769c50f674a)
* [Haskellでファイル操作する方法](https://qiita.com/uchiko/items/38a6d847b17f3b0a9861)