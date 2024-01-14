---
title:                "Gleam: ディレクトリが存在するかどうかをチェックする"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Gleamプログラミングブログ：ディレクトリの存在を確認する

## Why

ディレクトリの存在を確認する理由はさまざまです。例えば、ファイルを保存する前に既存のディレクトリかどうかを確認することで、重複したファイルを作成することを防ぐことができます。また、特定のディレクトリが存在しない場合には、自動的に作成することもできます。ディレクトリの存在を確認することで、よりスムーズなプログラミングを実現することができます。

## How To

以下のようなGleamコードを使用することで、ディレクトリの存在を確認することができます。

```Gleam
import gleam/fs

let dir_exists = fs.dir_exists("./my_directory")

if dir_exists {
    // ディレクトリが存在する場合の処理
} else {
    // ディレクトリが存在しない場合の処理
}
```

上記のコードでは、`./my_directory`パスに存在するディレクトリが`dir_exists`変数によって確認されています。もしディレクトリが存在する場合は、条件分岐の最初のブロックが実行され、存在しない場合は2番目のブロックが実行されます。

## Deep Dive

ディレクトリの存在を確認する方法は、環境によって異なります。例えば、WindowsとUnix系のシステムでは、ディレクトリの存在を確認するために使用されるメソッドが異なります。しかし、Gleamの`fs`モジュールを使用することで、プラットフォームに依存しないコードを書くことができます。

また、ディレクトリのパスを指定する際には、相対パスを使用することもできます。`./`はカレントディレクトリを表し、`../`は1つ上のディレクトリを表します。

## See Also

- [Gleamの公式ドキュメント：fsモジュール](https://gleam.run/modules/gleam/fs.html)
- [StackOverflowの同様の質問](https://stackoverflow.com/questions/23035053/how-to-check-if-a-directory-exists-in-a-shell-script)