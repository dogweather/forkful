---
title:                "Go: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ

ディレクトリの存在をチェックする理由は多々ありますが、主な理由はプログラムのスムーズな実行や不要なエラーの回避です。

## 方法

ディレクトリの存在をチェックする最も簡単な方法は、 `os.Stat()` 関数を使用することです。以下のコードを用いて、指定したパスにディレクトリが存在するかどうかを確認することができます。

```Go
package main

import (
    "os"
    "fmt"
)

func main() {
    dirPath := "path/to/directory"

    // ディレクトリが存在するかどうかを確認
    if _, err := os.Stat(dirPath); os.IsNotExist(err) {
        fmt.Printf("ディレクトリ %v は存在しません", dirPath)
    } else {
        fmt.Printf("ディレクトリ %v は存在します", dirPath)
    }
}
```

上記のコードを実行すると、指定したパスにディレクトリが存在する場合は「ディレクトリ (パス) は存在します」と表示され、存在しない場合は「ディレクトリ (パス) は存在しません」と表示されます。

## ディープダイブ

`os.Stat()` 関数は、指定したパスがディレクトリであるかどうかを確認するだけでなく、様々な情報を取得することもできます。例えば、ファイルサイズや作成日時などの情報を取得することができます。

また、ディレクトリの中に存在するファイルやサブディレクトリを取得する方法として、`os.ReadDir()` 関数を使用することもできます。以下のコードは、指定したパス内に存在するファイルやサブディレクトリの一覧を取得し、画面に表示する例です。

```Go
package main

import (
    "os"
    "fmt"
)

func main() {
    dirPath := "path/to/directory"

    // ディレクトリ内のファイルやディレクトリを取得
    fileInfos, err := os.ReadDir(dirPath)
    if err != nil {
        fmt.Printf("ディレクトリ内のファイルやディレクトリを取得できませんでした。エラー: %v", err)
    }

    // 取得したファイルやディレクトリを画面に表示
    fmt.Println("ディレクトリ内のファイルやディレクトリ:")
    for _, fi := range fileInfos {
        fmt.Println(fi.Name())
    }
}
```

上記のコードを実行すると、指定したパス内のファイルやサブディレクトリが画面に表示されます。

## 類似の記事

- [Golangでファイルを読み込む方法](https://example.com/golang-read-file)
- [Go言語でファイルやフォルダを作成する方法](https://example.com/golang-create-file-folder)
- [Golangでファイルやフォルダを削除する方法](https://example.com/golang-delete-file-folder)