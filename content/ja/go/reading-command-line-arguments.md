---
title:                "Go: コンピュータプログラミングの記事タイトル：コマンドライン引数の読み込み"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み込むことの重要性について説明します。Go言語でプログラムを書く際には、コマンドライン引数を利用することでより柔軟なプログラムを作ることができます。

## 方法
以下に、Go言語でコマンドライン引数を読み込む方法を示します。コードブロック内には、実際に動作するコードとその出力結果を示します。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// コマンドライン引数を1つずつ取得し、配列として保存する
	args := os.Args
	// args配列の要素を順に表示する
	for i := 0; i < len(args); i++ {
		fmt.Println(args[i])
	}
}
```

コードの実行結果：

```
./program
arg1
arg2
arg3
```

Go言語のosパッケージには、コマンドライン引数をより簡単に扱うための関数も用意されていますので、ぜひ調べてみてください。

## ディープダイブ
コマンドライン引数を読み込む場合、注意しなければいけない点があります。例えば、ユーザーがコマンドライン引数を指定しなかった場合、エラーが発生する可能性があることや、引数の型が文字列であることに気をつける必要があります。さらに、必要に応じてコマンドライン引数をパースして必要な情報を抽出することもできます。

## 参考リンク
- [Go言語でosパッケージを使ってコマンドライン引数を扱う方法](https://neptune.work/isbnos/see-also-golang-os-package)
- [コマンドライン引数を扱う際の注意点](https://ichi.pro/golangno-komandorainen-iryo-surutameni-tyuui-gakunennmonaiseimono-31401203456217)
- [コマンドライン引数のパース方法の例](https://tutorialedge.net/golang/parsing-command-line-flags-go/)