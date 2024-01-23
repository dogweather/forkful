---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
正規表現は文字列を検索・置換するパターンです。速くて柔軟だから、テキスト処理やデータ検証でプログラマがよく使います。

## How to: / どうやって：
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// 正規表現のパターンを定義
	pattern := "\\bhello\\b"
	input := "hello world, hello Go, hallo world"

	// 正規表現オブジェクトを作成
	re := regexp.MustCompile(pattern)

	// マッチする部分を探す
	matches := re.FindAllString(input, -1)
	fmt.Println(matches) // ["hello", "hello"]

	// 文字列を置換
	replaced := re.ReplaceAllString(input, "hi")
	fmt.Println(replaced) // hi world, hi Go, hallo world
}
```

出力:
```
[hello hello]
hi world, hi Go, hallo world
```

## Deep Dive / 詳細:
正規表現は1960年代に発明されました。`regexp`パッケージ以外に`bytes`や`strings`パッケージも似た機能を提供しますが、パターンマッチング能力に劣る。Go言語の正規表現はRE2エンジンをベースにしていて速くて安全。

## See Also / 参照:
- Go言語の正規表現パッケージ: https://pkg.go.dev/regexp
- 正規表現のテストと練習: https://regex101.com/
- RE2正規表現エンジン: https://github.com/google/re2
