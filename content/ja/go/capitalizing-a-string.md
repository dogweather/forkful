---
title:                "Go: 文字列の大文字化"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることについて、なぜこうするのかを説明します。文字列を大文字にすると、フォーマットや表示には大きな影響があります。例えば、文書内に見出しや特別な文言を含める際に、大文字にすることで目立たせることができます。

## 方法

以下の`Go`コードを使って、文字列を大文字にする方法を説明します。

```Go
func capitalize(str string) string {
  return strings.ToUpper(str)
}

func main() {
  stringToCapitalize := "hello world!"
  fmt.Println(capitalize(stringToCapitalize))
}
```

上記の例では、`hello world!`という文字列を大文字に変換しています。コードを実行すると、出力結果は`HELLO WORLD!`となります。

## もっと深く

文字列の大文字化について、もっと詳しく説明します。関数`ToUpper()`は、`Go`言語の`strings`パッケージに存在します。この関数は、文字列を全て大文字に変換することができるだけではなく、特定のロケールに基づいて変換を行うこともできます。また、文字列に含まれる英数字や記号なども大文字に変換することができます。

## 参考文献

"大文字化 - 文字列" Goドキュメント (https://golang.org/pkg/strings/#ToUpper)

## その他の関連記事

- "Go言語の基本的な文字列操作方法" (https://qiita.com/syossan27/items/0e57b18a8fa1fa94bfda)
- "文字列の大文字/小文字変換" (https://qiita.com/daijinload/items/e3ba179d9a43903bea8e)
- "文字列を大文字に変換する方法" (https://programming.guide/go/capitalize-strings.html)