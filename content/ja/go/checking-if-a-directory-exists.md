---
title:                "Go: 「ディレクトリが存在するかどうかを確認する」"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することの利点は、ファイルを操作する前に、ファイルが存在するかどうかを確認することで、より安全なプログラミングを実現できることです。

## 方法
ディレクトリが存在するかどうかを確認するには、osパッケージの `Stat`関数を使用します。以下の例では、ディレクトリが存在する場合は`true`を、存在しない場合は`false`を出力します。

```Go
import "os"

func main() {
    // ディレクトリのパスを指定
    dir := "/home/user/example"
    
    // `Stat`関数を使用してディレクトリの情報を取得
    _, err := os.Stat(dir)
    
    // エラーがない場合はディレクトリが存在すると判定
    if err == nil {
        fmt.Println("ディレクトリが存在します")
    } else {
        fmt.Println("ディレクトリは存在しません")
    }
}
```

## ディープダイブ
`Stat`関数は、指定したファイルやディレクトリの情報を取得する関数です。もしディレクトリが存在しない場合、`Stat`関数はエラーを返します。したがって、`Stat`関数を使用し、ディレクトリが存在するかどうかを確認することで、エラーを事前に防ぐことができます。

## 詳細を見る
- [Go言語でディレクトリを作成する方法](https://example.com/how-to-create-directory-in-go)
- [Go言語でファイルの存在をチェックする方法](https://example.com/how-to-check-if-file-exists-in-go)