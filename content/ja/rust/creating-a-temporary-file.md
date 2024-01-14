---
title:    "Rust: 「一時ファイルの作成」"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は様々あります。例えば、ファイルを安全に保存したり、一時的なデータを処理したりするために使用されます。Rustでは、一時ファイルを簡単に作成できる方法があります。ここでは、その方法について紹介します。

## 作り方

Rustでは、`std::fs`モジュールを使用して一時ファイルを作成します。以下のコードを実行すると、一時ファイルを作成することができます。

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("temp.txt").unwrap();
    file.write_all(b"Hello, world!").unwrap();
}
```

上記のコードでは、`temp.txt`という名前の一時ファイルを作成し、文字列を書き込んでいます。特に必要がなければ、作成した一時ファイルはプログラムの終了と同時に自動的に削除されます。

## 深堀り

一時ファイルを作成する際には、さまざまなオプションがあります。例えば、作成するディレクトリを指定したり、ファイルのモードを設定したりすることができます。また、一時的なデータを読み書きするための`std::io::TempDir`や`std::io::TempFile`といった便利なラッパーも提供されています。さらに詳しい情報は、公式ドキュメントを参照してください。

## 参考リンク

- [Rust公式ドキュメント：std::fsモジュール](https://doc.rust-lang.org/std/fs/index.html)
- [Rust公式ドキュメント：tmpモジュール](https://doc.rust-lang.org/std/io/temp/index.html)