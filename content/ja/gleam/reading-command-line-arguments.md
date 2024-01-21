---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:17.149306-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むとは、ユーザーまたは他のプログラムからの入力をプログラムに与える方法です。プログラマーは、動的にプログラムの振る舞いを変えたり、必要な情報を取得するためにこれを行います。

## How to: (やり方)
```gleam
import gleam/io
import gleam/os

pub fn main() {
  let args = os.args() // コマンドライン引数を取得
  match args {
    [] -> io.println("引数がありません。")
    [first | _rest] -> io.println("最初の引数: " ++ first)
  }
}
```
実行例:
```shell
$ gleam run my_app arg1
最初の引数: arg1
```

## Deep Dive (深掘り)
コマンドライン引数を読む概念はUNIXの初期から存在しています。Gleamでは、標準ライブラリ内の`gleam/os`モジュールがこの機能を提供します。他言語では`argv`や`argc`がこれらの引数を扱うために使われますが、Gleamでは単純に`os.args()`を呼び出すだけです。このリストを処理して、プログラムが必要とする情報を得ます。コマンドラインツールを作る場合には、引数を適切に解析するロジックが必要ですが、Gleamでは`gleam/string`やパターンマッチングを使うことで簡単にできます。

## See Also (関連リンク)
- Gleamの公式ドキュメント: [https://gleam.run/book/](https://gleam.run/book/)
- コマンドライン引数の解析に関するブログ記事: 取り扱われることは少ないかもしれませんが、さらなる情報を希望する場合はこのトピックに関するブログ記事を探してみてください。