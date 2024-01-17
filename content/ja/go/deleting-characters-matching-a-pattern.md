---
title:                "パターンに一致する文字を削除する"
html_title:           "Go: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何をするのか?  ##

削除する文字のパターンに一致する文字を削除するとは何か、それをプログラマーが行う理由は何かを2~3文で説明します。

## 方法:

以下のコードブロックには、Go言語を使った削除のコーディング例と結果が含まれています。

```
// 文字列から "a" を削除する
str := "banana"
result := strings.Replace(str, "a", "", -1)
fmt.Println(result)
// 結果は "bnn" となります
```

```
// 正規表現を使って文字列から数字を削除する
import "regexp"
str := "abc123def456"
re := regexp.MustCompile("[0-9]+")
result := re.ReplaceAllString(str, "")
fmt.Println(result)
// 結果は "abcdefgh" となります
```

## 詳細を掘り下げる:

(1) 文字の削除の歴史的背景は、様々なプログラミング言語やテキストエディタで利用されてきました。一般的な正規表現を使用することで、より柔軟に文字列のパターンを削除できるようになりました。 (2) 別の方法としては、文字列から特定の文字を置換するというものがあります。Go言語では、標準ライブラリの`strings`パッケージに組み込まれており、より効率的に文字列の削除が可能です。 (3) 実装の詳細については、Go言語のドキュメントやソースコードを参照することができます。

## 関連情報を参照してください:

- [Go言語ドキュメント](https://golang.org/doc/)
- [stringsパッケージ](https://golang.org/pkg/strings/)
- [正規表現について詳しく学ぶ](https://www.regular-expressions.info/)