---
title:    "Go: 「テキストファイルの作成」"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜテキストファイルを書くのか

Go言語でテキストファイルを書くことは、情報を保存するための非常に便利な方法です。テキストファイルは、データを簡潔にまとめて保存し、後で再利用することができます。また、コードやドキュメントを共有する際にも役立ちます。

## 書き方

テキストファイルを書くには、まず```Go ... ```コードブロック内にデータを入力します。そして、ファイル名を指定して、ファイルを保存するコマンドを入力します。次に、ファイルを開いてデータを書き込むためのループ処理を追加します。最後に、ファイルを閉じるコマンドを入力します。

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    file, err := os.Create("sample.txt") // ファイル作成
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close() // ファイルを閉じる
    data := []byte("こんにちは、世界！\n")
    for i := 0; i < 5; i++ { // ループ処理で5行を書き込む
        file.Write(data)
    }
}
```

上記のコードを実行すると、新しいテキストファイルが作成され、5行の「こんにちは、世界！」という文が書き込まれます。

## 深く掘り下げる

テキストファイルを書くときに重要なことは、データのフォーマットを正しく決めることです。また、ファイルを開いたら必ず閉じるようにすることも重要です。ファイルを開いている間、他のプログラムが書き込んでしまう可能性もあるため、閉じることでデータの整合性を保つことができます。

# もっと詳しく知るには

- [Writing Files in Go](https://gobyexample.com/writing-files)
- [Creating and Closing Files in Go](https://www.calhoun.io/creating-and-closing-files-in-go/)
- [Go言語でファイルの読み書きを行う](https://qiita.com/ruiu/items/31e87834311dc4379c19)