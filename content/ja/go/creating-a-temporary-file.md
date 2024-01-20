---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時ファイルを作成するとは、一時的なデータ蓄積と予想外の結果を防ぐためのプロセスです。プログラマはデバッグや不要なディスク容量消費を減らすためにこれを行います。

## 使い方：

一時ファイルの作成はGoの `ioutil` パッケージを利用します：

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	tmpFile, err := ioutil.TempFile("", "sample-")
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println("Temporary File Created:", tmpFile.Name())
}
```

これは、指定された接頭辞（"sample-"）を使用して、一時ファイルを作成するシンプルなコードです。そして、作成した一時ファイルのパスを表示します。

## 深掘り：

一時ファイルの作成はLinuxプログラミングの一部として発展してきました。代替策として `os.CreateTemp` 関数も利用可能ですが、私たちは `ioutil.TempFile` の方が一般的に使用されていることを見てきました。

また、一時ファイル作成時、Goは系統的なアプローチを取ります。まず、一時ディレクトリの場所を確認し（UNIX系システムでは通常 "/tmp"）、その後一意性を保証するためランダムな文字列を接頭辞または接尾辞として用います。

## 参考資料：

Go基礎：[https://go.dev/tour/welcome/1](https://go.dev/tour/welcome/1)  
IO Utilパッケージの使い方：[https://pkg.go.dev/io/ioutil](https://pkg.go.dev/io/ioutil)  
OSパッケージの使い方：[https://pkg.go.dev/os](https://pkg.go.dev/os)