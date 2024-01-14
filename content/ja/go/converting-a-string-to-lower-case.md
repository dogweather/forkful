---
title:    "Go: 文字列を小文字に変換する"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ？

Go言語では、文字列を小文字に変換する必要がある場合があります。これは、テキストの整形や比較、検索など、多くのアプリケーションで必要とされる基本的な操作です。

## 方法

文字列を小文字に変換するには、stringsパッケージのToLower関数を使用します。例えば、次のように記述します。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, WORLD!"

	fmt.Println(strings.ToLower(str))
	// 出力: hello, world!
}
```

## 詳細を掘り下げる

ToLower関数は、与えられた文字列の全てのUnicode文字を小文字に変換します。Go言語では、UTF-8を使用して文字列を表現するため、Unicode文字を操作する際には注意が必要です。ToUpper関数も同じ方法で大文字に変換できます。

## See Also

- [strings.ToLower documentation](https://golang.org/pkg/strings/#ToLower)
- [strings.ToUpper documentation](https://golang.org/pkg/strings/#ToUpper)