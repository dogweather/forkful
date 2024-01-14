---
title:    "Go: テキストファイルの読み込み"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、プログラミングで非常に重要です。例えば、CSVファイルを読み込んでデータを処理するなど、さまざまな用途でテキストファイルが使用されます。この記事では、Go言語で簡単にテキストファイルを読み込む方法を紹介します。

## 方法

まずは`os`パッケージをimportし、`Open()`関数を使って対象のテキストファイルを開きます。次に、`bufio`パッケージを使ってバッファリングし、`Scanner`を使ってテキストファイルを1行ずつ読み込みます。`Scan()`を使用して読み込んだデータを文字列として取得し、`Split()`を使ってスペースごとに分割します。最後に、必要な処理を行うことができます。以下にサンプルコードを示します。

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("sample.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text() // 1行ずつ読み込み
        words := strings.Split(line, " ") // スペースごとに分割
        // 必要な処理を行う
        fmt.Println(words)
    }

    if err := scanner.Err(); err != nil {
        fmt.Println(err)
        return
    }
}
```

以上のコードを実行すると、テキストファイルの内容がスペースごとに分割され、出力されることがわかります。

## 深堀り

テキストファイルを読み込む際には、ファイルのエンコーディングにも注意が必要です。`Scanner`はデフォルトでUTF-8のエンコーディングを使用するため、別のエンコーディングを使用しているファイルを読み込む場合は、`bufio.NewReader()`を使って`Reader`を作成し、`SetEncoding()`を使って適切なエンコーディングを指定する必要があります。

また、テキストファイルの最後に改行があるかどうかも確認する必要があります。`bufio`パッケージの`ScanLines()`を使うことで、改行を無視することができます。

## 参考リンク

- [Go Documentation: os package](https://golang.org/pkg/os/)
- [Go Documentation: bufio package](https://golang.org/pkg/bufio/)
- [Go Documentation: strings package](https://golang.org/pkg/strings/)