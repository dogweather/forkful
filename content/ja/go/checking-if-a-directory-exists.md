---
title:                "ディレクトリが存在するかどうかを確認する。"
html_title:           "Go: ディレクトリが存在するかどうかを確認する。"
simple_title:         "ディレクトリが存在するかどうかを確認する。"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
ディレクトリが存在するかどうかをチェックすることは、プログラマーが特定の処理を実行する前に必要なステップです。ディレクトリが存在しない場合、プログラムがエラーを出力することを防ぐために、コード内で条件分岐を使用してディレクトリの存在を確認することが重要です。

## How to:
```Go
if _, err := os.Stat("directory"); os.IsNotExist(err) {
  fmt.Println("The directory does not exist.")
} else {
  fmt.Println("The directory exists.")
}
```
上記のコードは、ディレクトリが存在するかどうかをチェックする基本的な方法です。まず、`os.Stat()`関数を使用して指定したディレクトリの情報を取得し、`os.IsNotExist()`関数を使用してエラーが発生した場合にディレクトリが存在しないことを判断します。もしディレクトリが存在しない場合、エラーを出力するようにプログラムを設定しています。

## Deep Dive:
ディレクトリの存在をチェックすることが重要な理由の一つは、プログラムの安全性を確保することです。ディレクトリが存在しない場合、ファイルやデータを読み込もうとするとエラーが発生する可能性があります。しかし、ディレクトリの存在をチェックする方法は他にもあります。`os.Stat()`関数の代わりに、`os.Lstat()`や`os.FileInfo()`などの関数を使用することもできます。

また、ファイルのパーミッションや属性をチェックすることによって、ディレクトリの存在だけでなく、そのディレクトリが読み取り専用であるかどうかなどの情報を取得することも可能です。プログラマーは状況に応じて適切な方法を選択し、コードを設計する必要があります。

## See Also:
- [Go Documentation: os.Stat() function](https://golang.org/pkg/os/#Stat)
- [Go Documentation: os.FileInfo interface](https://golang.org/pkg/os/#FileInfo)
- [Go Documentation: os.Lstat() function](https://golang.org/pkg/os/#Lstat)