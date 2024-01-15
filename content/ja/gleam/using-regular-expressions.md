---
title:                "正規表現を使う"
html_title:           "Gleam: 正規表現を使う"
simple_title:         "正規表現を使う"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使う理由はたくさんありますが、主なもののひとつは文字列のパターンを検索や置換するためです。例えば、メールアドレスや電話番号の正しいフォーマットを見つけたり、不正な文字列を置き換えたりすることができます。

## 使い方

正規表現を使うためには、 ```Gleam regex``` モジュールを使用します。まずはパターンを指定し、検索や置換する文字列を与えます。下記の例では、```"Gleam"``` という文字列を ```"Gleamのプログラミング言語"``` という文字列に置き換えます。

```Gleam
regex
  |> Regex.replace_all("Gleam", "Gleamのプログラミング言語")
  |> Regex.run("Gleamのプログラミング言語") #=> True
  |> Regex.run("Gleam言語") #=> False
```
置換する文字列が見つかった場合は ```True```、見つからない場合は ```False``` を返します。

## ディープダイブ

正規表現は、より高度なパターンの検索や置換を可能にする強力なツールです。文字クラスや量指定子などの特殊文字を使うことで、より柔軟なパターンマッチングが可能になります。詳細な情報は[公式のドキュメント](https://hexdocs.pm/gleam/regex.html)を参照してください。

## See Also

- [Gleam公式ドキュメント](https://gleam.run/documentation)
- [正規表現チュートリアル (日本語)](https://www.javadrive.jp/regex/)
- [GleamのregexモジュールのAPIリファレンス (日本語)](https://gleam.run/documentation/0.14/libraries/regex.html)