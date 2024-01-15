---
title:                "「コマンドライン引数の読み取り」"
html_title:           "Fish Shell: 「コマンドライン引数の読み取り」"
simple_title:         "「コマンドライン引数の読み取り」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読むか

コマンドライン引数を読む理由は、プログラムをより柔軟かつ効率的に動かすためです。引数を使うことで、プログラムを実行する際に動作を変更することができます。

## 使い方

Fish Shellでコマンドライン引数を読むには、以下のようなコードを使います。

```Fish Shell
#!/bin/fish

# 引数の最初の文字を表示する例
echo $argv[1][1]
```

この例では、スクリプトファイルを実行する際に引数として渡された言葉の最初の文字が出力されます。例えば、スクリプトファイルを `arg_example.fish` という名前で保存し、ターミナルで `fish arg_example.fish hello` と入力すると、`e` が出力されます。

もう一つの例として、引数を数値として使う方法を紹介します。

```Fish Shell
#!/bin/fish

# 引数がある場合は、それを2倍にして出力する例
if count $argv > 0
  echo (math $argv[1] * 2)
end
```

上記の例は、引数がある場合はその数値を2倍にして出力します。例えば、`fish arg_example.fish 5` を実行すると、`10` が出力されます。

## ディープダイブ

コマンドライン引数を読むには、ディープダイブする必要はありません。しかし、より詳しく知っておくと、より多様な引数を使うことができます。

最も一般的なコマンドライン引数は、数値、文字列、フラグの3つです。数値であれば、`math` コマンドを使って数学演算を行うことができます。文字列であれば、一般的な文字列操作のコマンドを使うことができます。また、フラグであれば、`set` コマンドを使って変数として定義することができます。

必要に応じて、複数の引数を渡すこともできます。その場合は、`$argv` 変数を使用し、各引数にアクセスすることができます。

## 参考リンク

- [Fish Shellのドキュメント](https://fishshell.com/docs/current/cmds/set.html)
- [コマンドライン引数の取得方法](https://stackoverflow.com/questions/2882864/passing-command-line-arguments-in-fish-shell)
- [Fish Shellでの数値操作](https://fishshell.com/docs/current/cmds/math.html)

---
（日本語訳）

## なぜ読むのか？

コマンドライン引数を読む理由は、プログラムをより柔軟かつ効率的に動かすためです。引数を使うことで、プログラムを実行する際に動作を変更することができます。

## 読み方

コマンドライン引数を読むには、Fish Shellで以下のようなコードを使います。

```Fish Shell
#!/bin/fish

# 引数の最初の文字を表示する例
echo $argv[1][1]
```

例えば、スクリプトファイルを `arg_example.fish` という名前で保存し、ターミナルで `fish arg_example.fish hello` と入力すると、`e` が出力されます。

また、引数を数値として使うには、以下のように `math