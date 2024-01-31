---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:39:34.348648-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から引用符を取り除くとは、実際のテキストをラップしているそれらの厄介な二重引用符または一重引用符の文字を取り除くことを意味します。これは、データをサニタイズするため、解析エラーを防ぐため、または引用符の余計なフワフワを追加せずにテキストをさらなる処理のために準備するために行います。

## どうやって：

ここにGoで引用符を軽く排除する簡単な方法があります：

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Unquoted:", unquotedString)
}
```

出力はこのように引用符がすべてなくなって見えます：

```
Original: "Hello, World!"
Unquoted: Hello, World!
```

## 詳細解説

昔、データフォーマットと交換が標準化されていない時、文字列の中の引用符は混乱を引き起こす可能性がありました。それは今でも可能で、特にJSONでや文字列をデータベースに押し込む時にそうです。Goの`strings`パッケージは、ホワイトスペースだけでなく、あなたが好きではない任意の文字を削除する`Trim`関数を備えています。

なぜRegexではないのか？`Trim`は単純な作業には速いですが、あなたの文字列が変な場所で引用符とかくれんぼをしている場合、regexがあなたの重い火器かもしれません：

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

それは、はさみとチェーンソーの間の選択のようなものです；仕事に適したツールを選びましょう。

## 参照

`strings`パッケージとそのパワーツールについての詳細：
- [パッケージ strings](https://pkg.go.dev/strings)

Goで正規表現の力を駆使するには：
- [パッケージ regexp](https://pkg.go.dev/regexp)

文字列トリミングの哲学に深く潜りたい人へ：
- [Trim メソッド](https://blog.golang.org/strings)
