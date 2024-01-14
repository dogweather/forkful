---
title:                "Rust: 新しいプロジェクトを開始する"
programming_language: "Rust"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ始めるのか

Rustは、高いパフォーマンスとメモリ安全性を備えたプログラミング言語です。あなたが新しいプロジェクトを始めるなら、Rustを使うことでエラーを減らし、より信頼性の高いソフトウェアを作ることができます。

## やり方

まずは、Rustのコードを取得するための環境を整えましょう。公式サイトから、Rustの最新のリリースをダウンロードします。ダウンロードが完了したら、インストールを行い、ターミナルを開きます。そして、次のコマンドを入力します。

```Rust
$ rustc --version
```

これにより、Rustのバージョンが表示されます。もしどのバージョンも表示されない場合は、インストールが正常に行われなかった可能性がありますので、再度行ってください。

次に、新しいプロジェクトを作成します。Rustでは、Cargoと呼ばれるパッケージマネージャを使ってプロジェクトを作成することができます。ターミナルで次のコマンドを入力してください。

```Rust
$ cargo new my_project
```

すると、my_projectという名前の新しいフォルダが作成され、その中にプロジェクトのテンプレートが自動的に生成されます。

最後に、コードを書いて実行してみましょう。my_projectフォルダの中にあるsrcフォルダ内のmain.rsファイルを開き、次のコードを入力してください。

```Rust
fn main() {
    println!("こんにちは！");
}
```

このように、日本語を含むprintln!マクロを使うことで、コンソールに日本語が表示されることが確認できます。

## 深いダイブ

Rustで新しいプロジェクトを始めるには、さまざまなツールや機能が用意されています。例えば、コードフォーマッタやテストフレームワークなど、開発をより便利にするための機能もあります。また、Rustの学習のためには、ドキュメントやコミュニティのサポートも充実しています。必要に応じて、これらの機能や情報を活用し、より良いプロジェクトを作り上げましょう。

## 併せて見てね

- [Rust公式サイト](https://www.rust-lang.org/)
- [Rustコミュニティフォーラム](https://users.rust-lang.org/)
- [Rustパッケージクレート](https://crates.io/)
- [Rustlings - はじめてのRust](https://github.com/rust-lang/rustlings)