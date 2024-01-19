---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Go: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認するとは、ファイルシステム上に特定のディレクトリがあるかをプログラムでチェックすることです。これを行う理由は、ファイルやサブディレクトリを作成、読み取り、移動する前に、その操作が可能であることを確認するためです。

## どうやるのか：

Rust言語でディレクトリ存在の確認作業を行うには、「std::path::Path」モジュール内の 'exists'関数を使用します。以下にその使用例を示します。

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");

    if path.exists() {
        println!("The directory exists.");
    } else {
        println!("The directory does not exist.");
    }
}
```

これを実行すると出力は以下のようになります（ディレクトリが存在する場合）:

```
The directory exists.
```

## 詳細:

* 履歴的な文脈: 最初から存照存在チェックは一般的なプログラミングタスクであり、ほとんどの言語において何らかの形でサポートされている。
* 代替案: 例えば、「std::fs::metadata」関数もディレクトリの存在を確認するために使えますが、これは追加のメタデータ情報を提供します。必要に応じて使い分けると良いでしょう。
* 実装の詳細: 'exists'関数は、内部的にはOSのAPIを呼び出して実装されています。UNIX系のシステムでは 'stat' システムコールが、Windowsでは 'GetFileAttributes' APIが使用されています。

## 関連リンク:

* Rustの公式ドキュメンテーションを参照してください: [Path](https://doc.rust-lang.org/std/path/struct.Path.html) と [std::fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html) の詳細情報を探し、使用方法を学んでください。
* ファイルシステムについてはこちらのリンクも参考になるでしょう: [Understanding Linux File System](https://www.linuxhandbook.com/linux-directory-structure/)