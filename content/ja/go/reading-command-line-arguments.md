---
title:    "Go: コンピュータプログラミングの記事タイトル：コマンドライン引数の読み込み"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読むことが重要な理由は、共通のソフトウェア開発プラクティスである「設定構成」によって異なるパラメーターをプログラムに渡すことを可能にすることです。Go言語では、コマンドライン引数を簡単に処理できる強力な方法が提供されています。

## 使い方

コマンドライン引数を読むには、まずosパッケージをインポートします。次に、`args := os.Args`を使ってプログラムに渡された引数を取得します。最後に、`args[index]`という形式を使って特定の引数を取得することができます。以下の例では、`go run main.go hello world`というコマンドを実行した場合、`args[0]`は"main.go"、`args[1]`は"hello"、`args[2]`は"world"になります。

```Go
package main

import "fmt"
import "os"

func main() {
	args := os.Args
	fmt.Println("最初の引数:", args[0])
	fmt.Println("2番目の引数:", args[1])
	fmt.Println("3番目の引数:", args[2])
}
```

実行結果:

```
最初の引数: main.go
2番目の引数: hello
3番目の引数: world
```

## ディープダイブ

上記の例では、コマンドライン引数を文字列として取得しましたが、Go言語では他の型にも変換することができます。例えば、`strconv`パッケージを使って数値や論理値に変換することができます。また、`flag`パッケージを使うとより高度な処理を行うことができます。これらの方法については、より詳細な[ドキュメント](https://golang.org/pkg/flag/)を参照してください。

## また見る

- [コマンドライン引数の処理（公式ドキュメント）](https://golang.org/pkg/os/#pkg-overview)
- [strconvパッケージのドキュメント](https://golang.org/pkg/strconv/)
- [flagパッケージのドキュメント](https://golang.org/pkg/flag/)