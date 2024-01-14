---
title:                "Go: コンピューターのプログラミング：コマンドライン引数の読み込み"
simple_title:         "コンピューターのプログラミング：コマンドライン引数の読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

こんにちは、Goプログラミングのファンの皆さん！

今日の記事では、コマンドライン引数の読み取り方について学びましょう。コマンドライン引数は、実行時にプログラムに渡す引数のことで、プログラムの実行をカスタマイズすることができます。

## Why

コマンドライン引数を読み取ることで、プログラムの実行の流れを制御したり、特定のデータを渡して処理を行ったりすることができます。例えば、ファイル名を引数として渡すことで、プログラムがそのファイルを開いて処理するように指示することができます。

## How To

コマンドライン引数を読み取るには、`os.Args`という関数を使用します。これは、プログラムを実行する際に渡される全ての引数を含む文字列の配列を返します。例えば、以下のように使用します。

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args
    fmt.Println(args)
}
```

上記のコードを実行すると、プログラムを実行した際に渡された全ての引数が表示されます。

```bash
$ go run main.go arg1 arg2 arg3
[$GOROOT/bin/go main.go arg1 arg2 arg3]
```

引数のうち、プログラム名は最初の要素として含まれていますので、必要であればスライスを利用して取り除く必要があります。

## Deep Dive

コマンドライン引数は、通常は文字列として渡されますが、必要に応じて数値やその他のデータ型に変換することもできます。また、フラグやオプションを指定することもできます。これにより、プログラムの挙動をさらに制御することができます。詳細な使い方については、[公式ドキュメント](https://golang.org/pkg/os/#Args)を参照してください。

See Also

- [公式ドキュメント: os.Args](https://golang.org/pkg/os/#Args)
- [Command-line arguments in Go](https://flaviocopes.com/go-command-line-arguments/)
- [Reading command line arguments in Go](https://www.digitalocean.com/community/tutorials/reading-command-line-arguments-in-go)