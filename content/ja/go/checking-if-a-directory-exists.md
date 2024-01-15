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

## なぜ
ディレクトリが存在するかどうかを確認する理由は、プログラムの実行やファイルの読み込みを行う前に、必要な場所やリソースが存在するかを確認するためです。

## 方法
ディレクトリが存在するかどうかを確認するには、以下のコードを使用します。

```Go
import "fmt"
import "os"

func main() {
    path := "/Users/username/Documents"
    
    // ディレクトリが存在するかチェックする
    if _, err := os.Stat(path); os.IsNotExist(err) {
        // ディレクトリが存在しない場合の処理
        fmt.Println("ディレクトリが存在しません")
    } else {
        // ディレクトリが存在する場合の処理
        fmt.Println("ディレクトリが存在します")
    }
}
```

上記のコードでは、まず`os`パッケージをインポートし、`path`変数に存在を確認するディレクトリのパスを設定します。その後、`os.Stat`を使用してディレクトリが存在するかどうかをチェックします。ディレクトリが存在しない場合、`os.IsNotExist`を使用してエラーを取得し、それに応じて処理を行います。

## ディープダイブ
ディレクトリをチェックする際に使用される`os.Stat`関数は、ファイルまたはディレクトリの情報を返すため、より詳細なチェックを行うことも可能です。たとえば、ファイルの所有者や更新日時などの情報を取得することができます。

また、ディレクトリの存在をチェックするためにもう1つの方法として、`filepath`パッケージの`Exists`関数を使用する方法があります。この関数は、ファイルパスが存在するかどうかを確認して`bool`型で結果を返します。

## 参考リンク
- [Go ドキュメント - os パッケージ](https://golang.org/pkg/os/)
- [Go ドキュメント - filepath パッケージ](https://golang.org/pkg/path/filepath/)
- [A Tour of Go - File Paths](https://tour.golang.org/flowcontrol/8)