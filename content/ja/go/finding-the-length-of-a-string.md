---
title:                "Go: 文字列の長さを見つける"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ

文字列の長さを調べることに巻き込まれる理由を説明します。

## 方法

文字列の長さを調べる方法について、Go言語のコーディング例と出力のサンプルをご紹介します。

```Go
package main

import "fmt"

func main() {
  str := "こんにちは、世界！"
  
  fmt.Println(len(str))
}

// 出力: 9
```

## 深堀り

文字列の長さを確認する方法について、より詳細な情報をお伝えします。文字列の長さを調べる際に考慮すべきことや、その他の役立つ情報をご紹介します。

## 参考リンク

[Go言語ドキュメンテーション](https://golang.org/doc/)
[Go言語チュートリアル](https://tour.golang.org/welcome/1)
[Go言語の文字列操作方法](https://www.tutorialspoint.com/go/go_string_manipulation.htm)