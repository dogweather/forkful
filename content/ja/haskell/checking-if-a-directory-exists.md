---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:56:52.424504-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するか確認することは、ファイルシステムに対するクエリです。プログラムがリソースを効率的に処理し、エラーを防ぐために行います。

## How to: (方法)
Haskellでは、ディレクトリが存在するかどうかを確認するには`System.Directory`モジュールを使用します。

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "some/directory/path"
  directoryExists <- doesDirectoryExist dirPath
  putStrLn $ "Directory exists: " ++ show directoryExists
```

実行結果は存在する場合に`True`、存在しない場合に`False`を出力します。

```
Directory exists: True
```
または
```
Directory exists: False
```

## Deep Dive (詳細解説)
`doesDirectoryExist`関数はIOモナド内で動作し、I/O操作の一部としてディレクトリの存在を確認します。この関数は、POSIX系システムでの過去の実装方法や、Windows APIの呼び出しに基づいています。代替手段として、直接ファイルパスにアクセスしてエラーキャッチを行う手法もありますが、`doesDirectoryExist`はより安全で簡潔です。なお、並列性やキャッシュの影響で、結果は100%の確実性を保証するものではありません。

## See Also (関連情報)
- [`System.Directory` モジュールのドキュメント](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [`FilePath` ライブラリについての情報](https://hackage.haskell.org/package/filepath)
- [Haskell IOに関するチュートリアル](http://learnyouahaskell.com/input-and-output)
