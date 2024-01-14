---
title:                "Gleam: 新規プロジェクトを始める"
simple_title:         "新規プロジェクトを始める"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ

新しいプロジェクトを開始する理由はたくさんあります。それは新しいアイデアを実現するためや、自分のスキルを磨くためなどでしょう。Gleamプログラミング言語を使うことで、新しいプロジェクトを始めるのがさらに簡単になります。

## 作り方

まず、Gleamのインストールから始めましょう。[https://gleam.run/getting-started/](https://gleam.run/getting-started/) を参照してインストール手順を確認し、ローカル環境にGleamを準備しましょう。次に、新しいプロジェクトフォルダを作成し、`gleam new`コマンドを使ってプロジェクトの骨組みを作ります。

```Gleam
# ターミナルで実行
mkdir new_project
cd new_project
gleam new my_project
```

これで`my_project`という名前の新しいプロジェクトができました。次に、`src`フォルダ内にある`my_project.gleam`ファイルを編集し、自分のコードを書き込みましょう。例えば、以下のように`hello`関数を追加すると、ターミナルで`compile`コマンドを実行した後に`hello_world.beam`ファイルが生成され、コマンドライン引数を渡して実行することができます。

```Gleam
pub fn hello(name: String) {
    "Hello " <> name
}

pub fn start(args: List(String)) {
    case args {
        [] -> println(hello("World"))
        [name] -> println(hello(name))
    }
}
```

例：

```Gleam
# コンパイル
gleam compile

# 実行
gleam run hello_world.beam
# 出力： Hello World

gleam run hello_world.beam Will
# 出力： Hello Will
```

## 深堀り

新しいプロジェクトを開始する際に重要なのは、自分のアイデアをしっかりと形にすることです。Gleamでは、型安全なコードを書くことで、アイデアをより明確に表現することができます。また、Erlang仮想マシンを利用することで、高い信頼性を持つソフトウェアを作ることができます。詳細は[https://gleam.run/](https://gleam.run/) で確認してみてください。

## See Also

- [https://gleam.run/getting-started/](https://gleam.run/getting-started/)
- [https://gleam.run/documentation/](https://gleam.run/documentation/)
- [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)

みなさんもぜひGleamを使って新しいプロジェクトを始めてみてください。楽しく学べるプログラミング言語です！