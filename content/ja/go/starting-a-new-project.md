---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何？& なぜ？

新しいプロジェクトを開始するとは、プログラムの新しいコードベースを作成することです。 これは、新しい機能を開発または既存のソフトウェアを大きく改善するために行います。

## 実行方法 :

以下に示すコードは、Goで新しいプロジェクトを作成および実行する最も基本的なステップです：

```Go
// main.go
package main

import "fmt"

func main(){
fmt.Println("私の新しいプロジェクトへようこそ！")
}

```
これを終了したらterminalに"go run main.go"を打ち込み、結果を確認します。

```shell
$ go run main.go
私の新しいプロジェクトへようこそ！
```

## より深く

新しいプロジェクトを開始するという概念は、ソフトウェア開発の歴史の初めから存在しています。 Go言語自体は、ソフトウェア開発の繰り返しの問題を解決するためにGoogleで開発されました。 これには、コードの読みやすさ、パフォーマンス、並行性などが含まれます。

他の言語でも新しいプロジェクトを開始することは可能で、Python、Java、Rubyなどが例です。これらの言語ごとに、新しいプロジェクトの設立方法は異なります。

Go言語で新しいプロジェクトを開始するには、最初に"main"パッケージと、プログラムのエントリーポイントである"main"関数を作成します。 "package main"は、Goがエントリーポイントを確認するためのシグナルであり、"func main()" はプログラムが最初に実行する関数です。

## 参考文献

- Go公式ドキュメンテーション：https://golang.org/doc/
- Goプロジェクトの最初のステップ：https://golang.org/doc/code.html#Command
- Goの歴史と目標：https://talks.golang.org/2012/splash.article
- Turing達人のGo：https://www.turing.com/blog/5-reasons-why-we-switched-from-python-to-go/