---
title:                "文字列の連結"
date:                  2024-01-20T17:34:46.666802-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列の連結とは、単純に文字列をつなぎ合わせることです。プログラマーがこれを行う理由は、メッセージを動的に生成したり、データ形式を整えたりするためです。

## How to: (方法)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// '+'で連結
	hello := "こんにちは"
	world := "世界"
	message := hello + "、" + world + "！"
	fmt.Println(message) // 出力: こんにちは、世界！

	// fmt.Sprintfで連結
	newMessage := fmt.Sprintf("%s、%s！", hello, world)
	fmt.Println(newMessage) // 出力: こんにちは、世界！

	// strings.Builderで効率的な連結
	var builder strings.Builder
	builder.WriteString(hello)
	builder.WriteString("、")
	builder.WriteString(world)
	builder.WriteString("！")
	fmt.Println(builder.String()) // 出力: こんにちは、世界！
}
```

## Deep Dive (深掘り)
初期のGoバージョンから、"+"演算子は最もシンプルな文字列連結手段でした。しかし、多数の文字列を連結する際にはパフォーマンス問題があります。

`fmt.Sprintf`は型安全であり、異なる型を含む文字列を連結する際によく使われますが、内部的にはI/O操作が多いため、"+"より遅い場合があります。

`strings.Builder`がGo 1.10で導入され、パフォーマンスの最適化が行われました。内部でバッファを使用し、メモリ割り当てを最小限に抑えながら連結を行うため、大量の文字列を効率的に組み立てるのに適しています。

## See Also (関連情報)
- [Goの公式ドキュメント](https://golang.org/pkg/)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [Go Blog: Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)