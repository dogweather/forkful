---
title:                "Go: ランダムな数字を生成する"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することに興味がある方々は多いかもしれません。それは、おもしろいゲームやアプリを作るために必要だったり、統計的なデータ分析を行うために必要だったりするからです。Go言語では、簡単にランダムな数字を生成することができます。今回はその方法をご紹介します。

## 生成する方法

最も基本的な方法は、math/randパッケージを使用することです。以下のコードを実行すると、0以上10未満の整数がランダムで生成されます。

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	fmt.Println(rand.Intn(10))
}
```

ここで使用しているrand.Intn()関数は、引数に渡した数未満のランダムな整数を生成します。

さらに、より複雑なパターンでランダムな数字を生成することもできます。例えば、以下のコードでは、0以上10未満のランダムな整数を5回生成して表示することができます。

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().Unix()) //時刻をシードに使用する
	for i := 0; i < 5; i++ {
		fmt.Println(rand.Intn(10))
	}
}
```

rand.Seed()関数は、ランダムな数字を生成する際に使用する乱数の種を設定します。ここでは、現在の時刻を乱数の種として設定することで、よりランダムな数字を生成することができます。

## 深堀り

Go言語のmath/randパッケージは、擬似乱数（pseudo-random numbers）を生成するために線形合同法（linear congruential generator）というアルゴリズムを使用しています。このアルゴリズムは、設計や実装の容易さから広く使用されていますが、その一方で予測しやすいという問題点もあります。

そのため、本番環境でより高品質な乱数を必要とする場合には、crypto/randパッケージを使用することをお勧めします。このパッケージは、暗号論的に安全な乱数を生成するための様々なアルゴリズムを提供しています。

また、math/randパッケージを使用して生成された乱数は、プログラムを実行するたびに同じ結果が得られる可能性があります。そのような事態を避けるためにも、乱数の種を毎回変えるようにするなどの工夫が必要です。

## 参考リンク

- math/randパッケージのドキュメント：https://golang.org/pkg/math/rand/
- crypto/randパッケージのドキュメント：https://golang.org/pkg/crypto/rand/
- 擬似乱数生成アルゴリズムの詳細：https://en.wikipedia.org/wiki/Linear_congruential_generator

## 関連リンク

- https://golang.org/pkg/math/rand/
- https://www.golangprograms.com/go-language/math-packages.html
- https://dev.to/manoj_hanwani/intro-to-golang-math-random-package-efk