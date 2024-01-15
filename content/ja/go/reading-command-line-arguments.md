---
title:                "コンピュータープログラミングの記事タイトル: コマンドライン引数の読み込み"
html_title:           "Go: コンピュータープログラミングの記事タイトル: コマンドライン引数の読み込み"
simple_title:         "コンピュータープログラミングの記事タイトル: コマンドライン引数の読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜコマンドライン引数を読み込むのか

コマンドライン引数を読み込むことで、プログラムの実行時にユーザーからの入力を受け取ることができます。これにより、より柔軟で、ユーザーエクスペリエンスの向上にもつながるプログラムを作ることができます。

## コマンドライン引数を読み込む方法

Go言語では、`os`パッケージを使用することでコマンドライン引数を読み込むことができます。以下の例を参考にしてください。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	for i := 1; i < len(args); i++ {
		fmt.Println("引数", i, "：", args[i])
	}
}
```

実行結果：

```bash
$ go run main.go hello world
引数 1 : hello
引数 2 : world
```

## ディープダイブ

Go言語では、`flag`パッケージを使用することでもコマンドライン引数を読み込むことができます。`flag`パッケージを使用すると、より詳細なオプション設定やデフォルト値の設定などを行うことができます。

例：
```Go
package main

import (
	"flag"
	"fmt"
)

func main() {
	// デフォルト値の設定
	namePtr := flag.String("name", "World", "The name to be greeted.")

	// オプション設定
	numPtr := flag.Int("num", 1, "The number of greetings.")

	// コマンドライン引数を解析
	flag.Parse()

	// 解析した値を使用して処理を行う
	for i := 0; i < *numPtr; i++ {
		fmt.Println("Hello", *namePtr)
	}
}
```

実行結果：

```bash
$ go run main.go -name John -num 3
Hello John
Hello John
Hello John
```

## 関連リンク

- [osパッケージドキュメント](https://golang.org/pkg/os)
- [flagパッケージドキュメント](https://golang.org/pkg/flag)