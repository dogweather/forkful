---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、英字が全て小文字になるように変えることです。プログラマは、比較機能を実装したり、ユーザ入力を標準化したりするためにこれを行います。

## 使い方：

Goの内蔵パッケージ`strings`を使って文字列を小文字に変換します。

```Go 
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO, WORLD!"
	fmt.Println(strings.ToLower(str))
}
```

上記のコードを実行すると、小文字の`hello, world!`が出力されます。

## より深く：

過去には特定のオペレーティングシステムでは大文字と小文字を区別しないファイル名が含まれていました。そのため、文字列を小文字に変換することはプログラマが共通の基盤で作業をするための重要な方法となりました。

小文字への変換の代替手段としては、全て大文字への変換も有ります。しかし、視覚的な一貫性と可読性のため、一般的に小文字が使用されます。

Goでは、`strings.ToLower`関数がUnicode文字を小文字に変換します。これは下層で`unicode.ToLower`関数を使用しています。この関数は指定された符号ポイントの小文字を返します。もし小文字版が存在しない場合は、関数は元の符号ポイントをそのまま返します。

## 参考資料:

1. [Go公式ドキュメンテーション](https://golang.org/pkg/strings/#ToLower)
2. [Unicodeの公式ドキュメンテーション](https://unicode.org/standard/standard.html)