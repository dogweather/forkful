---
title:                "「テキストファイルの作成」"
html_title:           "Go: 「テキストファイルの作成」"
simple_title:         "「テキストファイルの作成」"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書くのに参加する理由は、自分のコードやテキストを保存、管理、共有するためです。

## 方法

### Go言語でテキストファイルを書く方法

テキストファイルを書くためには、ファイルを作成し、書き込み用に開き、データを書き込み、最後に閉じる必要があります。Go言語では、`os`パッケージの`Create()`や`Open()`、`WriteString()`、`Close()`などを使用してこれらの作業を行うことができます。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// ファイルを作成
	file, err := os.Create("sample.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close() // 関数が終了するときにファイルを閉じる

	// 書き込み用にファイルを開く
	file, err = os.OpenFile("sample.txt", os.O_RDWR, 0644)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// データを書き込む
	_, err = file.WriteString("Hello, world!")
	if err != nil {
		panic(err)
	}
}
```

### サンプルの出力

上記のコードを実行すると、`sample.txt`という名前のファイルが作成され、`Hello, world!`というテキストが書き込まれます。

## ディープダイブ

### テキストファイルのパーミッション

ファイルを作成するときに指定する`0644`という数字は、テキストファイルのパーミッションを示しています。ここでは、6桁の数字が使用されていますが、最上位の4桁はファイルのタイプを示し、下3桁はパーミッションを示します。この例では、ファイルのタイプは`0644`なので、通常のファイルであり、下3桁は`644`という意味になります。この数字には4種類のアクセス権があり、それぞれ左から「所有者」、「グループ」、「その他のユーザー」を表しています。`0`が拒否、`1`が実行権限、`2`が書き込み権限、`4`が読み込み権限を表します。つまり、`0644`という数字は所有者には書き込み権限、グループとその他のユーザーには読み込み権限を与えることを意味します。

## 参考リンク

- [Golang Tutorial: Writing Files in Go](https://www.youtube.com/watch?v=IYf6aJpaYWs)
- [os package](https://golang.org/pkg/os/)
- [File Permissions in Unix and Linux](https://www.guru99.com/file-permissions.html)

See Also

[もっと学ぶ:](https://golang.org/) 更に多くのGo言語についての情報を入手するためのリンク集。