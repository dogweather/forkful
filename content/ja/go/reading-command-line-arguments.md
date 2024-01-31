---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:20.416421-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
コマンドライン引数とは、プログラム実行時にユーザーが提供するパラメータのことです。プログラマーは、様々な状況下で動作をカスタマイズするため、これらの引数を使用します。

## How to: (やり方)
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:]
	fmt.Println("Command Line Arguments:", args)
	if len(args) > 0 {
		fmt.Println("First Argument:", args[0])
	}
}
```
出力例:
```
$ go run myprogram.go arg1 arg2
Command Line Arguments: [arg1 arg2]
First Argument: arg1
```

## Deep Dive (深掘り)
コマンドライン引数はUNIXの初期から存在します。`os.Args`を使うのがGo言語のスタンダードですが、フラグ解析をするには`flag`パッケージが有用です。`os.Args`は、`main`関数が起動するときに自動的に生成されるスライスです。このスライスには、プログラム名が`os.Args[0]`として含まれ、残りが実際の引数です。

また、環境変数を読み取るためには`os.Getenv`や`os.LookupEnv`が役立ちます。引数と環境変数の主な違いは、引数が一時的な設定に適しているのに対し、環境変数はより永続的な設定に使われる点です。

## See Also (関連リンク)
- Go言語ドキュメントの`os`パッケージ: https://pkg.go.dev/os
- `flag`パッケージについてのGo言語ドキュメント: https://pkg.go.dev/flag
- コマンドライン引数についてより深く学ぶためのチュートリアル: https://gobyexample.com/command-line-arguments
- 環境変数を使ったプログラミングの例: https://gobyexample.com/environment-variables
