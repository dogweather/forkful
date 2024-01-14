---
title:                "Go: テキストファイルの書き方"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜ

テキストファイルを書くことに関わるメリットは多数あります。それらのメリットの１つに、プログラミング言語であるGoを使用してテキストファイルを作成することで、コードを読みやすく保守性の高いものにすることができるということが挙げられます。

## 方法

Goを使用してテキストファイルを書く方法は簡単です。まず最初に、テキストファイルを作成するために使用する`file`変数を宣言します。その後、作成したいテキストを`file.WriteString()`関数を使用して書き込みます。最後に、`file.Close()`関数を呼び出してファイルを閉じます。

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // テキストファイルの作成
    file, err := os.Create("sample.txt")
    if err != nil {
        fmt.Println(err)
    }

    // テキストの書き込み
    _, err = file.WriteString("こんにちは、世界！")
    if err != nil {
        fmt.Println(err)
    }

    // ファイルを閉じる
    file.Close()
}
```

上記のコードを実行すると、カレントディレクトリに`sample.txt`という名前のテキストファイルが作成され、ファイルには"こんにちは、世界！"というテキストが書き込まれます。

## ディープダイブ

Goを使用してテキストファイルを書く方法は、データの永続性を確保するために重要です。テキストファイルを使用することで、生成されたデータを後で調べることができ、データの変化を追跡することもできます。

また、ファイルを書き込む際にはエラー処理を行うことも重要です。エラー処理を行うことで、ファイルの書き込みや閉じる際に問題が発生した場合でも適切に対処することができます。

## 参考リンク

- [Go言語でテキストファイルを操作する方法](https://www.golangprograms.com/go-language/examples/53-golang-write-file)
- [Go言語でのエラーハンドリング](https://go-tour-jp.appspot.com/methods/12)
- [エンコーディングの設定](https://golang.org/pkg/io/ioutil/#TempFile)