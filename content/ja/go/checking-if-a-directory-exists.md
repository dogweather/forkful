---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Go: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？
ディレクトリが存在するかを確認するとは、特定のディレクトリパスが存在するかを検証するプログラムの一部です。これが重要な理由は、ディレクトリの存在が事前に保証されていないときに、エラーを引き起こす可能性があるからです。

## 実行方法
以下にGo言語でディレクトリが存在するかどうかを確認する一例を示します。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	_, err := os.Stat("ディレクトリパス")

	if os.IsNotExist(err) {
		fmt.Println("ディレクトリは存在しません")
	} else {
		fmt.Println("ディレクトリは存在します")
	}
}
```
出力例:

```
ディレクトリは存在します
```

## ディープダイブ
(1) ディレクトリが存在するかどうかを確認するという概念は、初期のプログラミングから存在しています。これは基本的なファイル操作メソッドの一部で、プログラムが安定的に動作するために重要です。

(2) 同じ問題を解決する代替案としては `os.IsExist(err)` やその他のパッケージを使用する方法があります。しかし、 `os.Stat` と `os.IsNotExist` は標準パッケージで、シンプルなため最も一般的に使用されます。

(3) 実装に関しては `os.Stat` はディレクトリの情報を返し、エラーが無ければディレクトリが存在すると見なします。 `os.IsNotExist(err)` がtrueを返す場合はディレクトリが存在しないと判断されます。

## 関連見
- Go言語の公式ドキュメンテーション: [osパッケージ](https://golang.org/pkg/os/)
- Go言語の公式ドキュメンテーション: [エラーハンドリング](https://blog.golang.org/error-handling-and-go)
これらのリンクは、さらなる情報と具体的な例を提供します。