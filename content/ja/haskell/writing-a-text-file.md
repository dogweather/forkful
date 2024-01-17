---
title:                "テキストファイルの作成"
html_title:           "Haskell: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

ファイルを書くというのは、プログラマーがテキストファイルに情報を保存することを指します。これは、プログラマーが実行したコードやデータを保存するために必要です。

## How to:

Haskellでテキストファイルを書くには、以下のコードを使用します。

```
import System.IO 

main = do 
    let fileName = "sample.txt" 
    let content = "This is a sample text file." 
    writeFile fileName content 
    putStrLn "File written successfully."
```

このコードを実行すると、`sample.txt`という名前のテキストファイルが作成され、中には `This is a sample text file.`という文章が保存されます。また、最後の行では、コンソールにメッセージが表示されます。

## Deep Dive:

テキストファイルを書く機能は、Haskellの標準ライブラリで使用できる `System.IO` モジュールを介して提供されます。この機能を使用することで、コードを実行する際に生成されるデータや結果を永続的に保存することができます。

代替方法としては、バイナリファイルを作成し、データを格納することもできますが、テキストファイルを使用することでデータをより見やすく、編集しやすくすることができます。

ファイルの書き込み機能は、ファイルの作成やデータの保存という基本的なタスクを実行するためのものですが、より詳細なファイル操作や管理をするには、`System.Directory` モジュールや `Data.ByteString` パッケージを使用することができます。

## See Also:

- [System.IO Documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [System.Directory Documentation](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Data.ByteString Package](https://hackage.haskell.org/package/bytestring)