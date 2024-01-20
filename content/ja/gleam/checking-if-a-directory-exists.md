---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:56:16.399306-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかどうかを確認するとは、ファイルシステム上のフォルダが実際に存在するかをチェックする処理です。この確認は、ファイル操作前のエラー防止や、動的にファイルシステムを扱う際に重要です。

## How to:
```gleam
// Gleam module importing functions for dealing with the file system
import gleam/io

pub fn check_directory_exists(dir: String) -> Bool {
  // Try to read the directory and return true if it exists
  case io.read_dir(dir) {
    Ok(_) -> true
    Error(_) -> false
  }
}

// Sample usage
fn main() {
  let directory = "some/directory/path"
  let exists = check_directory_exists(directory)
  io.println(exists) // prints "true" if directory exists, "false" otherwise
}
```

## Deep Dive (深い潜行)
Gleamのファイルシステム機能は、Elixir言語やその他のErlang VM言語に触発されています。`io.read_dir`はディレクトリを読み込むための関数で、成功すれば`Ok`の値が、失敗すれば`Error`の値が返されます。代わりに`file.stat`や`file.exists`のような関数を使うことも可能ですが、直接ディレクトリ内のファイルを読む動作の結果に基づいた判断が一般的です。

## See Also (関連するリンク)
- Gleam's official website and documentation: [https://gleam.run/](https://gleam.run/)
- Erlang's :file module, for more file system operations: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Elixir's File module, for further understanding of file handling in the BEAM ecosystem: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)