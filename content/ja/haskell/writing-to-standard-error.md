---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

category:             "Haskell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラー (stderr) への書き込みは、エラーメッセージや警告をログに出力する手法です。これにより、正常な出力と診断メッセージを分離し、デバッグやログ監視を効率的に行えます。

## How to:

```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "エラー: 不明なコマンド"
```

実行結果: 標準エラーへ「エラー: 不明なコマンド」と表示されます。

## Deep Dive
Haskellの`Stdlib`では、`System.IO` モジュールが標準出力(stdout)と標準エラー(stderr)の両方を扱います。初期のUnixシステムで採用されて以来、stderrはエラーメッセージの標準的な出力先です。代替手段として日誌ファイセルへ直接記録することもありますが、標準エラーへの書き込みはデバッグ時の標準プラクティスです。実装上、`hPutStrLn` 関数を使用し、第一引数に `stderr`ハンドルを指定することで標準エラーに情報を出力できます。

## See Also
- [Hackage Haskell Library documentation for System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell Wiki on IO](https://wiki.haskell.org/IO_inside)
- [Real World Haskell: Chapter 7 - I/O](http://book.realworldhaskell.org/read/io.html)
