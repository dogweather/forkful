---
title:    "Go: 一時ファイルの作成"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

ゴー言語を学ぶ上で、一時ファイルの作成が必要な場面があるかもしれません。この記事では、一時ファイルの作成がなぜ必要なのかを紹介します。

## 作り方

一時ファイルを作成するには、まず `ioutil` パッケージをインポートし、`ioutil.TempFile` 関数を使います。以下のコード例は、一時ファイルを作成し、書き込み、読み込んで削除するプログラムです。

```Go
package main

import (
"fmt"
"io/ioutil"
)

func main() {
    // TempFile関数を使って一時ファイルを作成
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        fmt.Println(err)
    }
    defer tempFile.Close()

    // 一時ファイルに書き込み
    _, err = tempFile.WriteString("Hello, Go!")
    if err != nil {
        fmt.Println(err)
    }

    // 一時ファイルを読み込み
    fileContents, err := ioutil.ReadFile(tempFile.Name())
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println(string(fileContents))

    // 一時ファイルを削除
    err = ioutil.Remove(tempFile.Name())
    if err != nil {
        fmt.Println(err)
    }
}
```

実行結果は以下のようになります。

```
Hello, Go!
```

## 深堀り

一時ファイルは、長期間保存する必要がないデータを一時的に作成・保存するために使われます。例えば、オンラインショッピングサイトでの仮の注文データの保存や、一時的なログファイルの作成などに使用されます。

また、一時ファイルを作成する際には、ファイルのパーミッションを設定することができます。デフォルトでは読み書きが可能なパーミッションが設定されますが、必要に応じてパーミッションを変更することができます。詳細な情報は公式ドキュメントを参照してください。

## 参考リンク

- [Go言語の公式ドキュメント - ioutilパッケージ](https://golang.org/pkg/io/ioutil/)
- [Go 言語で一時ファイルを作る方法](https://qiita.com/rrrfff/items/375f487bff5f4cda3919)