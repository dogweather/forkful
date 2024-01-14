---
title:                "Rust: 一時ファイルの作成"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作成することには、多くの理由があります。例えば、プログラムが一時的に必要なデータを格納するため、またはプログラム実行中にファイルを作成する必要がある場合などです。

## 作り方
Rustでは、標準ライブラリの`std::fs`モジュールを使用することで、簡単に一時ファイルを作成することができます。まず、`use std::fs::File`で`File`型をインポートします。その後、`File::create("/path/to/file")?`という形式でファイルを作成し、必要に応じて書き込みや読み込みを行います。

```Rust
use std::fs::File;

let mut temp_file = File::create("/path/to/file")?;
// ファイルに書き込みを行うコード
// ...

// ファイルの読み込みを行うコード
// ...
```

まず、`File::create()`メソッドで一時ファイルを作成し、その後ファイルに書き込みや読み込みを行うためのコードを追加します。コードを実行すると、指定したパスに一時ファイルが作成され、処理が終わると自動的に削除されます。

## 深堀り
一時ファイルを作成する際、ファイルのパーミッションや開くモードを指定することもできます。また、`File`型の他にも`TempDir`型を使用することで、一時的なディレクトリを作成することもできます。詳細な情報については、公式ドキュメントを参照することをおすすめします。

## 参考リンク

- [Standard Library - `std::fs`](https://doc.rust-lang.org/std/fs/index.html)
- [Creating a Temporary Directory with Rust](https://www.fluvio.io/docs/architecture/code/creating-temporary-directory)
- [Filesystem operations in Rust - File and directories](https://dev.to/lukbh/speed-up-filesystem-operations-in-rust-1c8f)