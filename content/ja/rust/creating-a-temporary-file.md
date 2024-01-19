---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
一時ファイルはプログラムが短時間だけ使用する、通常は隠されたファイルです。これらは主に大量のデータの一時的な保存場所として、または同時に多数のユーザーによるファイル書き込みを回避するために使われます。

## 使い方:
Rustで一時的なファイルを作成するには `tempfile` クレートが必要です。以下に基本的な使用方法を示します。
```Rust
use std::io::Write;
use tempfile::tempfile;

let mut tmpfile = tempfile().unwrap();

write!(tmpfile, "Hello world!").unwrap();
```
このコードは一時的なファイルを作成し、"Hello world!"と一つのメッセージを書き込みます。

## ディープダイブ:
昔ながらのプログラミング言語、例えば、CやPerlでは `tmpfile` 関数を使って一時ファイルを作成します。Rustはこの機能を `tempfile` クレートで提供し、より簡単かつ安全に一時ファイルを扱うことができます。

他の選択肢としては、自分で一時ファイルを管理することも可能ですが、安全性や便利性のためには `tempfile` の使用をお勧めします。

`tempfile` クレートはOSの一時ディレクトリ（Windowsなら `C:\Users\YourName\AppData\Local\Temp` など）に一時ファイルを作成します。ファイル名はランダムに生成され、在中プロセスの終了と共に自動的に削除されます。

## 参考資料:
Rustの一時ファイル作成に関連する詳細な情報をお求めの方は、以下のリンクをご参照ください。

- [tempfile - Github](https://github.com/Stebalien/tempfile)
- [Rust Documentation](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rustのファイル操作まとめ](http://rust-lang.github.com/book/ch12-02-reading-a-file.html)