---
title:                "Go: 「文字列の大文字化」"
simple_title:         "「文字列の大文字化」"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# なぜ

文字列を大文字にする必要性について、Goプログラミングの世界に興味を持っている読者の方々に向けて説明をします。

## 方法

文字列を大文字にするには、stringsパッケージのToTitle関数を使います。

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	word := "hello"
	fmt.Println(strings.Title(word))
}

// Output: Hello
```

## 深堀り

上記の例では、ToTitle関数を使って文字列を大文字にすることができます。また、stringsパッケージにはToTitleSpecialという、より柔軟に文字列を扱うことができる関数もあります。

# 参考リンク

- [Goでの文字列操作方法](https://golang.org/pkg/strings/)
- [stringsパッケージのドキュメント](https://pkg.go.dev/strings?utm_source=godoc)
- [フリーのGoコース](https://www.udemy.com/course/go-the-complete-developers-guide/) 

# 参考文献

- ["Hello"を"HELLO"に変換する方法](https://golangdocs.com/golang-capitalizing-first-letter-of-string)
- [stringsパッケージのドキュメントの例](https://golang.org/pkg/strings/#ToTitle)