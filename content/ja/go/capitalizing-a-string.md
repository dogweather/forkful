---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を大文字化するって？単純に、文字列の中の小文字を全部大文字に変換することだよ。なぜやるの？読みやすさを上げたり、データを統一するためだね。

## How to: (実装方法)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "こんにちは、世界！"
	upper := strings.ToUpper(original)
	fmt.Println(upper)
}
```
実行結果:
```
こんにちは、世界！
```

## Deep Dive (深掘り)
Goの`strings.ToUpper`関数を使うと、簡単に文字列を大文字にできるよ。実は、Go言語における大文字化は、Unicodeを完全にサポートしてる。だから、多言語の文字も正しく大文字になる。細かい実装詳細は、Goのソースコードを見ると良い理解が得られるよ。以前のプログラミング言語では、ASCIIコードのみをサポートしてたけど、Goはグローバルな使用を考慮して作られているからね。

もし手軽に使える他の方法を探しているなら、`bytes`パッケージもあり、`bytes.ToUpper`という関数でbyte sliceを大文字化することが可能だよ。

## See Also (参考文献)
- Goの公式ドキュメント: [strings package](https://pkg.go.dev/strings)
