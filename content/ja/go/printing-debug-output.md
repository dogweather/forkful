---
title:                "デバッグ出力のプリント"
html_title:           "Go: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグの際、デバッグ出力をプリントする理由は何でしょうか？

デバッグ出力は、プログラムの実行中にコードの動作を確認し、問題を特定するために使用されます。不具合やエラーを特定するのに役立つので、開発者にとって非常に重要なツールです。

## 使い方

デバッグ出力をプリントするには、 `fmt` パッケージの `Println` 関数を使用します。下の例では、変数 `num` に値を代入し、その値を出力しています。 

```Go
num := 5
fmt.Println("The value of num is:", num)
```

出力結果は以下のようになります。

```
The value of num is: 5
```

また、デバッグ出力を繰り返しプリントしたい場合は、 `for` ループを使用することもできます。次の例では、`num` の値を1から10までプリントしています。

```Go
for i := 1; i <= 10; i++ {
  fmt.Println("The value of num is:", i)
}
```

出力結果は以下のようになります。

```
The value of num is: 1
The value of num is: 2
The value of num is: 3
The value of num is: 4
The value of num is: 5
The value of num is: 6
The value of num is: 7
The value of num is: 8
The value of num is: 9
The value of num is: 10
```

## ディープダイブ

デバッグ出力をより効果的に行うためには、出力に変数や式を含めることもできます。これにより、その時点での変数の値や式の評価結果を確認することができます。例えば、次のコードでは、変数 `num1` と `num2` の積をプリントしています。

```Go
num1 := 5
num2 := 6
fmt.Println("The product of num1 and num2 is:", num1 * num2)
```

出力結果は以下のようになります。

```
The product of num1 and num2 is: 30
```

また、デバッグ出力を使用する際は、不要な出力が多すぎると読みづらくなる可能性があるので、適切なタイミングで出力を制限することも重要です。Go言語には、`log` パッケージを使用してログを記録する機能もありますので、必要に応じてそちらを使用することもできます。

## 参考リンク

- [The Go Programming Language](https://golang.org/)
- [Go 言語でのデバッグの方法](https://qiita.com/SyoUeki/items/9e5be502f8bc144f666f)
- [Effective Go](https://golang.org/doc/effective_go.html#debugging) 

## 関連リンク

- [Go 言語でのエラーハンドリングの基本](https://qiita.com/tenntenn/items/19b1466190d365f0811c)
- [Go 言語における単体テストの書き方](https://qiita.com/greymd/items/795241c67745f9c02b3d)