---
title:                "コマンドライン引数の読み込み"
html_title:           "Gleam: コマンドライン引数の読み込み"
simple_title:         "コマンドライン引数の読み込み"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

コマンドライン引数を読み取るとは、プログラマーが実行時にコマンドラインから渡された引数をプログラム内で利用することです。この機能を利用することで、プログラムをより柔軟にカスタマイズでき、ユーザーにとっても使いやすいものになります。

## 方法：

```
Gleam.attach_arguments()
```

上記のように、```attach_arguments()``` 関数を使うことで、コマンドラインで渡された引数をプログラム内で利用することができます。以下は、実際のコーディング例です。

```
import gleam/inspect

fn main(args: List(String)) {
    // 第1引数を取得する
    argument1 = args |> List.head(|> unwrap_or("default_value"))

    // 第2引数を取得する
    argument2 = args |> List.tail() |> List.head(|> unwrap_or("default_value"))

    // 全ての引数を出力する
    gleam/inspect.p(format!("Arguments: {}", args))
}
```

## 深掘り：

コマンドライン引数の機能は、プログラミングの世界では一般的であり、多くの言語でサポートされています。代替手段として、環境変数を利用する方法もありますが、コマンドライン引数の方がより直感的で使いやすいと言えます。Gleamでは、環境変数を扱うモジュールが提供されていますが、コマンドライン引数を読み取る方がよりシンプルなコーディングが可能です。

## 関連リンク：

- [Gleamの公式ドキュメント](https://gleam.run)
- [GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam)
- [Gleamでのコマンドライン引数の利用方法](https://gleam.run/articles/command_line_args)