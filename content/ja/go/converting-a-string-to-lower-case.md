---
title:    "Go: 「文字列を小文字に変換する」"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why
文字列を小文字に変換する理由は、基本的なデータ処理や文字列比較のためです。しかし、engagedエクストライセキヤ 用オプションの場合、特定の言語や場合によっては、大文字/小文字の統一のために必要な場合もあります。

## How To
文字列を小文字に変換するには、stringsライブラリのToLower()メソッドを使用します。下記のコードを参考にしてください。

```Go 
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "Hello, WORLD!"
	fmt.Println("Original string:", s)
	fmt.Println("Lowercase string:", strings.ToLower(s))
}
```
```
Original string: Hello, WORLD!
Lowercase string: hello, world!
```

## Deep Dive
ToLower()メソッドは、渡された文字列を全て小文字に変換します。このメソッドは、内部的にUnicode文字の正規化を行って、フォールバックできるようにしています。これにより、言語やコンテキストによって特定の文字が異なる小文字フォームを持つ場合にも、正しい変換が行われるようになります。

また、一部の言語や文字ではToLower()メソッドが動作しない場合もあります。そのような場合は、strings.ToLowerSpecial()メソッドを使用して、カスタムの処理を行うことができます。

## See Also
- [Go stringsライブラリドキュメント](https://golang.org/pkg/strings/)
- [Unicode正規化についての詳細な説明](https://unicode.org/reports/tr15/)

記事中でも触れましたが、Goのstringsライブラリには他にも便利なメソッドがたくさんありますので、是非公式ドキュメントを参考にしてお使いください。また、Unicodeの正規化については詳しく学んでおくことで、文字列処理における問題を回避することができます。