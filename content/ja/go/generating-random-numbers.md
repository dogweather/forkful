---
title:                "Go: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# なぜランダムな数値を生成するのか？

コンピューターの世界では、ランダムな数値を生成することがよく行われます。これは、ゲームやランダムイベントのシミュレーション、セキュリティーの為の暗号化など、さまざまな用途に使われます。Go言語を使ってランダムな数値を生成する方法について学びましょう！

## 生成方法

Go言語を使ってランダムな数値を生成するには、math/randパッケージを使います。まず、パッケージをインポートします。

```Go
import "math/rand"
```

次に、ランダムシードを設定します。これは、ランダムを生成する際に元となる値を指定するものです。例えば、現在の時間を指定することで、毎回異なるランダムな数値を生成することができます。

```Go
rand.Seed(time.Now().UnixNano())
```

最後に、生成したい数値の範囲を指定し、rand.Intn()関数を使ってランダムな数値を生成します。以下の例では、1から10までのランダムな数値を生成しています。

```Go
for i := 0; i < 10; i++ {
  fmt.Println(rand.Intn(10) + 1)
}
```

✨ 以下のような出力が得られます。

```
7
2
6
10
1
4
5
9
2
8
```

## ディープダイブ

Go言語のmath/randパッケージは、擬似乱数（pseudo-random）を生成します。つまり、ある種のパターンに基づいてランダムな数値を生成することになります。そのため、非常に長い期間にわたって同じパターンが繰り返される可能性があります。

しかし、より高度なランダム性が求められる場合は、crypto/randパッケージを使用することが推奨されています。このパッケージは、暗号的に安全な乱数（cryptographically secure random）を生成します。

また、Go言語では独自の乱数生成器を実装することも可能です。その場合、線形合同法やメルセンヌ・ツイスターなど、さまざまなアルゴリズムを用いることができます。

# 参考リンク

- [Go言語のmath/randパッケージドキュメント](https://golang.org/pkg/math/rand/)
- [Go言語のmath/randパッケージのソースコード](https://golang.org/src/math/rand/rand.go)
- [Effective Go - ランダムな数値の生成](https://golang.org/doc/effective_go.html#random-numbers)
- [Creating Good Test Seed Values with Golang](https://www.calhoun.io/creating-good-test-seed-values-in-go/)
- [Go言語のcrypto/randパッケージドキュメント](https://golang.org/pkg/crypto/rand/)
- [Cryptographically secure pseudorandom number generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)