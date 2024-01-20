---
title:                "正規表現の使用"
html_title:           "Gleam: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# ＃＃ 何となぜ？
正規表現を使うのは、プログラマーが特定のテキストパターンを検索や置換、バリデーションなどの作業をより効率的に行うためです。テキスト操作はプログラミングにおいて非常に重要であり、正規表現はそれをより高度にするために開発された便利なツールです。

# ＃＃ 試し方：
Gleamで正規表現を使用する場合は、まず ``` gleam/regex ``` ライブラリをインポートする必要があります。そして以下のようにコードを記述します。

``` gleam
let regex = Regex.new("l+[ea]")
let result = Regex.find(regex, "hello, world")
```

このコードは、文字列 ``` "hello, world" ``` の中から ``` l,e,lle,la,ell,elll ``` を検索し、その中で最初に見つかったものを返します。

# ＃＃ ディープダイブ：
正規表現は、最初にUnixのテキスト処理ツールとして開発されました。その後、多くのプログラミング言語に組み込まれるようになり、現在では非常に一般的なツールとなっています。まだ正規表現を使ったことがない場合は、パターンマッチングの代替として試してみることをお勧めします。

# ＃＃ 参考：
- [正規表現の基礎](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)