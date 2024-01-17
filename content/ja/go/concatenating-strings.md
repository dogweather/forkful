---
title:                "文字列の連結"
html_title:           "Go: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

文字列を連結することは、Go言語でよく使われるテクニックの一つです。プログラマーは、複数の文字列を一つの文字列にまとめることで、より効率的なコードを書くことができます。

## 方法：

以下の例では、文字列を連結する方法を示します。それぞれの例の出力結果も示します。

```
Go fmt.Println("こんにちは、" + "世界") → "こんにちは、世界"
```

```
Go name := "山田"
   age := 20
   fmt.Printf("%sは%d歳です。", name, age) → "山田は20歳です。"
```

## 詳細を掘り下げる

文字列の連結は、プログラミングの歴史の中でよく使われるテクニックです。他の方法としては、テンプレートや文字列フォーマットを使う方法もあります。Go言語では、`+`演算子を使うことで文字列を連結することができます。

## 関連リンク

- [Go言語ドキュメンテーション、文字列の連結](https://golang.org/pkg/strings/#Join)
- [Go言語ドキュメンテーション、フォーマット](https://golang.org/pkg/fmt/)