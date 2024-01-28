---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
テキストファイルを書くことは、データを永続化する行為です。プログラマはログ生成、設定保存、データ交換のためにテキストファイルを利用します。

## How to: (方法)
```Haskell
import System.IO

-- ファイル書き込み関数
writeFileExample :: IO ()
writeFileExample = do
    let content = "こんにちは、Haskell!\n"
    writeFile "greeting.txt" content

-- 実行結果
main :: IO ()
main = writeFileExample
```

`greeting.txt` が作成され、次の内容が含まれます:

```
こんにちは、Haskell!
```

## Deep Dive (深掘り)
Haskellでは、テキストファイルに書き込む標準的な方法は `writeFile` 関数です。この関数は過去から存在し、`System.IO` ライブラリーの一部です。`writeFile` の代替品としては、`appendFile`（ファイルの末尾に追加）、`hPutStr`（ファイルハンドルを使用）、`ByteString`や`Text`ライブラリを使った効率的な書き込みがあります。実装の詳細においては、`writeFile` はファイルを開き、データを書き込み、そして自動的にファイルを閉じます。

## See Also (関連情報)
- [Haskell Documentation on System.IO](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html)
- [Real World Haskell: Working with Files](http://book.realworldhaskell.org/read/io.html)
