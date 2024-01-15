---
title:                "テキストファイルの読み込み"
html_title:           "Go: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、プログラミングにおいて非常に一般的なタスクです。テキストファイルに保存されているデータをプログラムで処理したり、編集したりする際に必要になります。そのため、Goを勉強している人やすでにGoを使用している人は、テキストファイルを読み込む方法を知ることで、より多くのプログラミングスキルを身につけることができます。

## 使い方

テキストファイルをGoで読み込むには、まずファイルを開く必要があります。そのためには、`os.Open()`関数を使用します。以下は、その例です。

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("sample.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()
    // ここでファイルを処理するコードを記述します
    fmt.Println("ファイルを正常に読み込みました。")
}
```

これにより、`sample.txt`という名前のファイルを開いて、その内容を処理することができます。ファイルを扱う際は、必ず`defer file.Close()`を使用してファイルを閉じることを忘れないようにしましょう。

次に、ファイルの内容を読み取る方法を見ていきます。ファイルを読み取るには、`bufio`パッケージを使用して`Scanner`を作成し、`Scan()`メソッドを使用します。例えば以下のようになります。

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
        panic(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }
}
```

この例では、`sample.txt`の内容が1行ずつ出力されます。`Scan()`が`false`を返したとき、つまりファイルの最後まで読み込んだときにループから抜けます。

## ディープダイブ

ファイルを読み込む際、エラーの処理が重要になります。上記の例のように、`os.Open()`や`scanner.Err()`でエラーが発生した場合は、そのエラーを適切に処理する必要があります。また、ファイルを読み込む際のモードやバッファサイズの指定など、さまざまなオプションもあります。詳細については、公式ドキュメントを参照することをおすすめします。

## 参考リンク

- [os パッケージ - Go 言語公式ドキュメント](https://golang.org/pkg/os/)
- [bufio パッケージ - Go 言語公式ドキュメント](https://golang.org/pkg/bufio/)
- [Goでファイルを読み書きする - Qiita](https://qiita.com/ma91n/items/43e3b005c2502c3a5d6e)