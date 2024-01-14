---
title:                "Gleam: コンピュータプログラミングの記事のタイトル：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングの記事のタイトル：コマンドライン引数の読み取り"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#なぜ
コマンドライン引数を読む方法を学ぶことによって、あなたのGleamプログラミングのスキルを向上させることができます。また、より効率的なコードを書くことができるようになり、プログラムの実行をカスタマイズすることができるようになります。

##方法
コマンドライン引数を読むには、まず`Gleam.System`モジュールをインポートする必要があります。そして、`args`という関数を使用してコマンドライン引数を取得することができます。以下はコマンドライン引数を取得し、その内容を表示するコードの例です。

```Gleam
import Gleam.System

pub fn main(_) {
  args()
  |> System.args
  |> println
}
```

コマンドライン引数を指定するには、以下のように`gleam run`コマンドを使用します。

```
$ gleam run ファイル名 引数1 引数2 ...
```

例えば、上記のコードを`args.gleam`というファイル名で保存し、以下のように実行すると、

```
$ gleam run args.gleam hello world
```

以下のように表示されます。

```
[hello, world]
```

##深堀り
現在のプログラムがどの引数で実行されたかを知る必要がある場合、`Gleam.System`モジュールの`os_args`関数を使用することもできます。この関数は、実行中のプログラムと同じ引数のリストを返します。以下はその例です。

```Gleam
import Gleam.System

pub fn main(_) {
  let program = System.args()[1]
  os_args()
  |> List.drop(1) // プログラム名を除外する
  |> println
}
```

上記のコードを`program_args.gleam`というファイル名で保存し、以下のように実行すると、

```
$ gleam run program_args.gleam hello world
```

以下のように表示されます。

```
[hello, world]
```

##参考リンク
- [Gleam公式ドキュメント: Systemモジュール](https://gleam.run/book/stdlib.html#system)
- [Gleam公式ドキュメント: コマンドライン引数の取得](https://gleam.run/book/manual.html#command-line-arguments)
- [Gleam公式リポジトリのコマンドライン引数の使用例](https://github.com/gleam-lang/gleam/blob/master/examples/command-line-arguments.gleam)