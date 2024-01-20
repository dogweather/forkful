---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストの検索と置換は、一連の文字を別の文字列に置き換える開発者の一般的なタスクです。プログラマーはこれを行うのは、特定のパターンを修正、更新、または削除するためです。

## 使い方：

以下にGleamでテキスト検索と置換を行う方法の 例を示します：

```Gleam
import gleam/regex

fn main() {
  let re = regex.from_string("foo").unwrap()
  let new_string = regex.replace(&re, "foobar", "bar")
  assert new_string == Ok("barbar")
}
```
この例では、"foo"という文字列を"bar"に置換しています。

## ディープダイブ：

テキストの検索と置換は、機能のないプログラミング言語から、最も現代的なプログラミング言語まで組み込まれています。正規表現はこれらのタスクを行うためのパワフルなツールで、Gleamでは `gleam/regex` モジュールを通じて利用できます。

なお、Gleamの検索と置換はErlang VM上で実行されます。これは開発者により大きなパフォーマンスと安全性を提供します。

またErlang VMでは、compile time（コンパイルタイム）とruntime（ランタイム）のエラーを分離して取り扱います。

## 参考情報:

* 更に詳しい情報として、Gleamのドキュメンテーションをご覧ください：https://gleam.run/book/tour/regex.html 
* 置換操作についての詳細な情報はこちら：https://gleam.run/documentation-guides/gleam-for-elixir-people/#find-and-replace