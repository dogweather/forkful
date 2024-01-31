---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:31.149295-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイル読み込みって何？なぜ必要？プログラムがテキストファイルの内容を取得する作業を指します。設定、データのインポート、ログ解析など、多くの理由でプログラマはファイル読み込みを行います。

## How to:
```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("example.txt") // ファイルを開く
	if err != nil {
		panic(err)
	}
	defer file.Close() // 関数終了時にファイルを閉じる

	scanner := bufio.NewScanner(file)
	for scanner.Scan() { // ファイルを一行ずつ読み込む
		fmt.Println(scanner.Text()) // 行の内容を表示
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}
}
```
出力例：
```
こんにちは、世界！
これはテキストファイル読み込みの例です。
```

## Deep Dive
テキストファイルの読み込みはプログラミングの基本です。Goでのファイル読み込みは、`io`パッケージが提供する抽象化に基づいています。`bufio`を使用すると、効率的なバッファリングにより、大きなファイルもメモリに優しい方法で処理できます。過去には、C言語の`FILE`ポインタやJavaの`FileReader`のような異なるAPIが使われていましたが、Goのインタフェースはシンプルさと実用性で際立っています。一方、`io/ioutil`パッケージにあった`ReadFile`関数も便利でしたが、Go 1.16で`os`パッケージへ移動しました。

## See Also
- Go by Exampleのファイル読み込みガイド（英語）: https://gobyexample.com/reading-files
- Go公式ドキュメントの`io`パッケージ（英語）: https://golang.org/pkg/io/
- Go ブログ: ioutilパッケージの廃止とosパッケージへの移行について（英語）: https://blog.golang.org/io/ioutil-deprecation
