---
title:    "Go: 「デバッグ出力のプリント」"
keywords: ["Go"]
---

{{< edit_this_page >}}

## なぜ

なぜデバッグ出力を表示するのかを理解することは非常に重要です。デバッグ出力を使うことで、プログラムの実行中に発生したエラーや変数の値を確認することができます。特にコードの複雑な部分では、デバッグ出力を使用することでバグを見つけるのに大きな助けとなります。

## 方法

デバッグ出力を表示するには、Go言語の標準パッケージである`fmt`を使用します。以下のように`fmt.Println()`を使用して、文字列を出力することができます。

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hello, 世界")
}
```

このコードを実行すると、ターミナルに"Hello, 世界"という文字列が表示されます。また、`fmt.Print()`を使用することで改行せずに出力することもできます。

```Go
package main

import "fmt"

func main() {
    fmt.Print("Hello, ")
    fmt.Print("世界")
}
```

このコードを実行すると、"Hello, 世界"のように改行なしで文字列が表示されます。

## 深堀り

`fmt.Println()`や`fmt.Print()`には、複数の引数を与えることができます。これを使うと、変数の値や計算結果などを簡単に出力することができます。

```Go
package main

import "fmt"

func main() {
    name := "太郎"
    age := 20
    fmt.Println("私の名前は", name, "です。年齢は", age, "です。")
}
```

このコードを実行すると、"私の名前は太郎です。年齢は20です。"という文字列が表示されます。

また、`fmt.Printf()`を使用することで、より詳細な出力をすることもできます。以下の例では、`%s`と`%d`を使用して、変数の値を指定しています。

```Go
package main

import "fmt"

func main() {
    name := "太郎"
    age := 20
    fmt.Printf("私の名前は%sです。年齢は%dです。\n", name, age)
}
```

このコードを実行すると、"私の名前は太郎です。年齢は20です。"という文字列が表示されます。

## また見る

- [Go言語公式ドキュメント](https://golang.org/doc/)
- [Go言語チュートリアル](https://tour.golang.org/welcome/1)