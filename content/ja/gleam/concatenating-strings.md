---
title:                "文字列の連結"
html_title:           "Gleam: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## なにとなぜ？
文字列を連結するとは、簡単に言えば、２つの文を１つに結合することです。プログラマーがこれをする理由は、テキストを効率的に操作し、異なるメッセージを作成するためです。

## 手順：
下のように,```Gleam ... ```コードブロック内のコーディングの例と出力を見てみましょう。

```Gleam
let greeting = "Hello"
let name = "Sara"
let message = greeting <> name

IO.println(message)
```

出力：

```
HelloSara
```

## 深く掘り下げる：
（1）歴史的な背景：文字列の結合は、プログラミング言語で一般的に使われてきました。多くの言語では、基本的な機能として組み込まれています。
（2）代替手法：Gleamには、他にも使用できるテキスト操作のツールがあります。例えば、部分文字列を取得することができます。
（3）実装の詳細：Gleamでは、文字列を連結するために`<>`演算子を使います。この演算子は、左側の式に右側の式を追加して文字列を作成します。

## 参考：
関連する情報源へのリンク：
- [Gleamドキュメント：文字列の結合](https://gleam.run/documentation/guides-and-tutorials/concatenation)
- [Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)