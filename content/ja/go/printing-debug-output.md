---
title:                "デバッグ出力の印刷"
html_title:           "Go: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
デバッグ出力を表示することとは、プログラマーがコードの実行中に作成したデータやメッセージをターミナルやファイルに表示することです。プログラマーがデバッグ出力を使用する理由は、コードの実行中に発生するエラーやバグを特定し、それらを修正するために役立つからです。

## 方法：
```Go
fmt.Println("Hello, World!")
```
上記のコードを実行すると、ターミナルに「Hello, World!」という出力が表示されます。これはデバッグ出力の一例です。デバッグ出力をより詳細に表示したい場合は、```fmt.Printf```を使用することもできます。
```Go
age := 25
fmt.Printf("My age is %v years old.", age)
```
これにより、ターミナルには「My age is 25 years old.」という出力が表示されます。

## より詳しく：
デバッグ出力は、プログラム開発の初期に使用されていた手法の一つです。他の方法としては、デバッガーを使用したり、断片化したコードを実行することでエラーを発見する方法があります。しかし、デバッグ出力を使用することで、コード全体を実行しなくてもエラーを特定することができ、効率的なデバッグ作業が可能になります。

## 関連情報：
- [Golang fmtパッケージドキュメント](https://golang.org/pkg/fmt/)
- [デバッグ技術に関する日本語の記事](https://qiita.com/tags/debug)