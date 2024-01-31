---
title:                "リファクタリング"
date:                  2024-01-26T01:36:57.595621-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、外部の振る舞いを変えずに既存のコンピュータコードの構造を再編成するプロセスです。プログラマーは、ソフトウェアの非機能属性（読みやすさや保守性など）を改善するためにこれを行います。これにより、コードを理解しやすくし、複雑さを減らし、バグをより簡単に見つける助けとなります。

## 方法：
簡単なGoコードのリファクタリング例を試してみましょう。数字のスライスの平均を計算するスニペットを取り、明確さと再利用性のためにリファクタリングします。

オリジナルのコード:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

リファクタリングされたコード:
```Go
package main

import "fmt"

// CalculateAverageはfloat64のスライスを受け取り、平均を返します。
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

リファクタリングされたコードでは、平均を計算するロジックを`CalculateAverage`という別の関数に抽出しました。これにより、`main`関数はより簡潔になり、平均計算のロジックが再利用可能でテスト可能になります。

## ディープダイブ
コードのリファクタリングは現代の概念ではありません。広範なコンピュータの使用以前から存在します。その実践は、機械工学の領域や、それ以前の時代に始まった可能性があります。ソフトウェアでは、1990年代にオブジェクト指向プログラミングとエクストリームプログラミング（XP）の登場とともに、より体系化され、特にマーティン・ファウラーの画期的な本「リファクタリング：既存のコードの改善設計」の影響を受けました。

リファクタリング技術には数多くあり、変数の単純な名前変更から、メソッドやクラスの抽出のようなより複雑なパターンまであります。鍵となるのは、ソフトウェアの機能を変更せずに内部構造を改善する小さな、段階的な変更を加えることです。

Goを使用する場合、その言語の単純さと強力な標準ライブラリのおかげで、リファクタリングは比較的簡単になります。しかし、リファクタリングがバグを導入しないことを確かめるために、良い単体テストセットを持っていることが依然として重要です。`gorename`や`gofmt`のようなツールはプロセスの一部を自動化するのに役立ち、IDEはしばしば組み込みのリファクタリングサポートを持っています。

手動リファクタリングに加えて、Goのための自動コードリファクタリングツールがいくつか利用可能であり、例えばGoLandのリファクタリングツールやGo Refactorなどがあります。これらはプロセスを早めることができますが、コードを理解し、考え抜かれた変更を加える代わりにはなりません。

## 参照
 - [Goでのリファクタリング：シンプルは美しい](https://go.dev/blog/slices)
 - [Effective Go：インターフェースを使ったリファクタリング](https://go.dev/doc/effective_go#interfaces)
 - [マーティン・ファウラーのリファクタリングページ](https://refactoring.com/)
 - [GoLandリファクタリングツール](https://www.jetbrains.com/go/features/refactorings/)
