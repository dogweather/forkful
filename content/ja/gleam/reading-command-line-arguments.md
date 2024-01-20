---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数を読み取るとは、プログラムが起動時に受け取る追加情報のことを指します。これは、プログラムの動作を制御したり、特定のタスクを実行したりするために行います。

## やり方:

以下にGleamでのコマンドライン引数の読み取り方法を示します:

```Gleam
import gleam/option.{Some, None}

pub external fn command_line_args() -> list(Option(String)) =
  "erlang" "init:get_plain_arguments"
  
pub fn show_args() {
  let args = command_line_args()
  case args {
    [] -> io.println("No arguments found")
    _ -> io.println(args)
  }
}
```
実行後の出力は次のようになります:

```shell
$ gleam run my_program arg1 arg2
[Some("arg1"), Some("arg2")]
```

## 深入り:

歴史的な文脈としては、コマンドライン引数はUNIXシェルから派生し、初期のダイナミックなプログラミング操作に使用されました。今日ではほとんどの言語でサポートされています。

代替手段としては、環境変数や設定ファイルを使用してプログラムに情報を伝えることがあります。これは永続的な設定や秘密の保存に特に役立ちます。

Gleamでは、Erlangランタイムから直接コマンドライン引数を取得します。その結果、取得されたコマンドライン引数はGleamの強力なパターンマッチング機能と組み合わせて使用できます。

## 参考にして:

- [Gleamの公式ドキュメンテーション](https://gleam.run/documentation/)
- [コマンドライン引数についての総合ガイド](https://www.codecademy.com/articles/command-line-arguments)
- [Erlangのコマンドライン引数の取り扱い](http://erlang.org/doc/man/init.html#id98707)