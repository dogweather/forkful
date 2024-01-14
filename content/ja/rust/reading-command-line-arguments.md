---
title:    "Rust: コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み込むことの重要性をお伝えするために、今回はRustプログラミング言語での読み込み方を紹介します。

## ハウツー

コマンドライン引数を読み込むには、標準ライブラリの `env` モジュールを使用します。以下の例では、`std::env::args()` を使用してコマンドライン引数を受け取り、`println!` マクロを使用して出力します。

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("コマンドライン引数: {:?}", args);
}
```

実行すると、以下のような結果が得られます。

```
コマンドライン引数: ["target/debug/sample"]
```

そして、引数を使用してプログラムを実行する際には、`cargo run` コマンドを使用します。

さらに、コマンドライン引数に直接値を渡すこともできます。

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let first_arg = &args[1];
    println!("最初の引数: {}", first_arg);
}
```

実行結果:

```
最初の引数: hello
```

これで、コマンドライン引数を読み込む方法が分かりました。

## ディープダイブ

さらに、`std::env` モジュールには他にも便利な関数があります。例えば、`args_os()` を使用すれば、引数を `OsString` オブジェクトとして受け取ることができます。また、`current_dir()` を使用すれば、現在のディレクトリを取得できます。

また、環境変数を取得・設定する機能も `std::env` モジュールに備わっています。例えば、`var()` 関数を使用すると、指定した環境変数の値を取得できます。

コマンドライン引数や環境変数を使用することで、柔軟性の高いプログラムを作ることができます。ぜひ、試してみてください。

## 参考リンク

- [Rustプログラミング言語公式サイト](https://www.rust-lang.org/ja/)
- [std::env モジュール　ドキュメント](https://doc.rust-lang.org/std/env/index.html)
- [コマンドライン引数を使う方法](https://qiita.com/termoshtt/items/d099cd08dbe68270e32b)
- [Rustで環境変数を扱う方法](https://qiita.com/7ma7X/items/6fa51a74604b5d83576f)

# オススメのリンク

- [よく使う標準ライブラリの関数まとめ](https://qiita.com/__init__/items/f5e5d2556d36ac40dad2)
- [Rustプログラミングのコツとテクニック](https://qiita.com/Qui/items/d18e207e04b172c8f72d)