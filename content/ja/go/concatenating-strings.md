---
title:                "Go: 文字列の連結"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
Go言語で文字列を連結することに関心を持つのかについて、簡単に説明します。

## 方法
文字列を連結する方法について、コーディング例と出力サンプルを含めて ```Go ... ``` のコードブロックを使用して説明します。

```
// 文字列を連結する例
package main

import "fmt"

func main() {
    str1 := "Hello"
    str2 := "World"

    fmt.Println(str1 + " " + str2) // Output: Hello World
}
```

## 深堀り
Go言語で文字列を連結する方法の詳細について、より深く説明します。

文字列を連結するには、```+```演算子を使用します。また、複数の文字列を連結する場合は、```+```演算子を使い続けるよりも、```fmt.Sprintf()```関数を使用する方が効率的です。

さらに、Go言語では文字列の連結に```strings.Join()```関数を使うこともできます。この関数には、連結したい文字列のスライスと区切り文字を指定することができます。例えば、以下のようになります。

```
// 文字列のスライスを連結する例
package main

import (
    "fmt"
    "strings"
)

func main() {
    strs := []string{"Hello", "World"}

    fmt.Println(strings.Join(strs, " ")) // Output: Hello World
}
```

## See Also
参考になるリンクをいくつか紹介します。

- [The Go Blog: Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- [Go by Example: Strings](https://gobyexample.com/strings)
- [Go言語で配列やスライスを連結する方法](https://qiita.com/genkiroid/items/2b601c64ce35abbfaebc)