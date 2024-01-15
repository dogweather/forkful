---
title:                "一時ファイルの作成"
html_title:           "Go: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

クリーンアップを心配することなく、一時的なファイルを作成することができるからです。

## How To

一時的なファイルを作成するには、`ioutil` パッケージの `TempFile()` 関数を使用します。以下のコードは、カレントディレクトリに一時的なファイルを作成し、そのパスを出力するものです。 

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	file, err := ioutil.TempFile("", "temp_file")
	if err != nil {
		panic(err)
	}

	defer file.Close()

	fmt.Println("一時的なファイルのパス:", file.Name())
}
```

出力結果:
```
一時的なファイルのパス: C:\Users\Username\AppData\Local\Temp\temp_file633073148
```

一時的なファイルを作成する際には、`suffix` として空の文字列を指定することで、作成されるファイルにランダムな文字列が追加されます。また、必要に応じて `prefix` としてファイル名の先頭に追加する文字列を指定することもできます。

## Deep Dive

一時的なファイルはメモリを節約するために一時ファイルとして作成され、プログラムが終了した時点で自動的に削除されます。しかし、ファイル自体が自動的に削除されるわけではなく、プログラムがファイルを閉じた時点で削除されます。そのため、ファイルを使用し終わったら必ず `defer` を使用してファイルを閉じるようにする必要があります。

一時的なファイルが作成されるディレクトリは、空の文字列が指定されている場合は `os.TempDir()` が使用されます。また、デフォルトのファイルパーミッションは `0666` になりますが、最終的なパーミッションは実行環境によって異なる場合があります。

## See Also

- [ioutil パッケージドキュメント](https://golang.org/pkg/io/ioutil/)
- [一時的なファイルの作成と削除](https://www.callicoder.com/golang-create-write-and-delete-temporary-file/)
- [Go by Example: Temporary Files](https://gobyexample.com/temporary-files)