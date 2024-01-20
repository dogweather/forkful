---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りとは、プログラムが起動するときに引数を受け取ることです。それにより、プログラムの動作を柔軟に制御できます。

## 方法：

以下にGo言語を使ってコマンドライン引数を読み取る例を示します。

```Go
package main

import (
   "fmt"
   "os"
)

func main() {
   arguments := os.Args
   for i := range arguments {
       fmt.Println(arguments[i])
   }
}
```

このコードを実行すると、コマンドライン引数が順番に表示されます。第一引数はプログラム自体のパスです。

## ディープダイブ：

1. **歴史的な背景**：Unix時代から、シェルスクリプトやC言語などの言語ではコマンドライン引数が使われてきました。Goはこの慣行を受け継いでいます。

2. **代替手段**：フラグパッケージは、コマンドライン引数のパースを更に簡単にします。例えば、型チェックやデフォルト値の設定が可能です。

3. **実装詳細**：os.Argsはstring型のスライスです。os.Args[0]はプログラムの名前、os.Args[1:]が実際の引数です。

## 参考文献：

1. [osパッケージの公式ドキュメンテーション](https://golang.org/pkg/os/)
2. [flagsパッケージの公式ドキュメンテーション](https://golang.org/pkg/flag/)