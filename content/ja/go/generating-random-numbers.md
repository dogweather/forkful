---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:01.929515-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
ランダム数の生成って？それはプログラムで予測不可能な数字を作ることです。なぜこれが必要かというと、ゲーム、セキュリティ、データ解析など様々な場面で必要不可欠だからです。

## How to:
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// ランダムソースの初期化
	rand.Seed(time.Now().UnixNano())

	// 0から99までのランダムな数
	randomNumber := rand.Intn(100)
	fmt.Println("ランダム数:", randomNumber)

	// 1.0未満のランダムな浮動小数点数
	randomFloat := rand.Float64()
	fmt.Println("ランダム浮動小数点数:", randomFloat)
}
```
実行例（実行するたびに違う結果が得られます）：
```
ランダム数: 42
ランダム浮動小数点数: 0.812889
```

## Deep Dive
ランダム数の生成には歴史があります。たとえばUNIXの時代から`rand()`関数がありました。Goでは、`math/rand`パッケージを使用します。これは擬似ランダム数を生成するため、`rand.Seed()`でシード値を設定することが重要です。本当の乱数を必要とするなら、`crypto/rand`パッケージから生成できます。こちらは暗号セキュリティレベルのランダム数です。

## See Also
- Goの公式ドキュメント `math/rand`: https://golang.org/pkg/math/rand/
- 暗号セキュリティレベルの乱数について `crypto/rand`: https://golang.org/pkg/crypto/rand/
- 「擬似乱数」についてのウィキペディアのページ: https://ja.wikipedia.org/wiki/擬似乱数
