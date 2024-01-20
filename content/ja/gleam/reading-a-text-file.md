---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Gleamでテキストファイルの読み込み: コードリーディーガイド

## なんで＆なぜ？
テキストファイルの読み込みとは、コンピュータに格納されたテキスト情報をコードで取得することを指します。これによって、プログラマーはさまざまな情報を操作し、分析することができます。

## 使い方
Gleamでは、テキストファイルの読み込みは非常に短い行で行うことができます。

```Gleam
import gleam/otp/process
import gleam/otp/mailbox
import gleam/io

fn read_file(file_path: String) -> Result(String, Nil) {
  let mailbox = mailbox.new()
  process.start_link(
    fn() {
      let result = io.file.read_to_string(file_path)
      mailbox.send(result)
    },
  )
  mailbox.receive()
}
```
ファイルパスを指定して関数を呼び出すと、以下のような出力が得られます。

```Gleam
let result = read_file("path_to_file.txt")
assert Ok(file_contents) = result
io.println(file_contents)
```

## ディープダイブ
テキストファイルの読み込みは、プログラミングの基本的なタスクで、歴史的にはファイルシステムが発明された時から存在します。Gleamでは、Erlang/OTP のプロセスとメールボックスを使用して、ファイルの読み込みを並行して実行することができます。これは、大量のファイルを読み込む際にパフォーマンスを向上させます。ただし、ファイルが存在しない場合や読み込みに失敗した場合は、エラーを返すようにします。

他の言語では、異なるライブラリや機能を使って同じタスクを行います。例えば、Pythonなら`open()`関数、Javaなら`Scanner`クラスや`FileReader`クラスを利用します。Gleamの場合，最も顕著な違いは，並行プログラミングをファーストクラスの機能としてサポートしていることです。

## 関連リンク
* [gleam/otp ドキュメンテーション](https://hexdocs.pm/gleam_otp/)
* [gleam/io ドキュメンテーション](https://hexdocs.pm/gleam_io/)
* [Erlang/OTPドキュメンテーション](https://erlang.org/doc/apps/stdlib/io_protocol.html)