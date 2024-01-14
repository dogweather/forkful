---
title:    "Gleam: コマンドライン引数の読み込み"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読むのか

コマンドライン引数を読む必要は、コンピュータープログラミングにおいて非常に重要です。コマンドライン引数を使用することで、プログラムを簡単にカスタマイズしたり、さまざまなオプションを提供したりすることができます。この記事では、Gleamプログラミング言語を使用して、コマンドライン引数を読み取る方法を紹介します。

## 読み取り方法

コマンドライン引数を読み取るには、`Command` モジュールを使用します。まず、コマンドライン引数を読み取るための `parse` 関数を宣言します。

```Gleam
import gleam/command

command.parse(args, []) 
```

この関数では、`args` というリストを引数として受け取ります。`args` には、実際のコマンドライン引数が格納されます。また、空のリスト `[]` を引数として渡します。これは、オプションを指定することで、より詳細な読み取りを行うことができるようになります。

次に、`Command.parse` 関数を使用して、コマンドライン引数を解析します。

```Gleam
let { "arg1", "arg2" } = command.parse(args, []) 
```

この例では、コマンドライン引数の最初の2つを `arg1` と `arg2` という変数に格納しています。

## 深堀り

`Command` モジュールには、より詳細なオプションが用意されています。例えば、`parse_required` 関数を使用することで、必須の引数を指定することができます。

```Gleam
let { "arg1", "arg2" } = command.parse_required(args, ["required_arg1", "required_arg2"]) 
```

また、`parse_args` 関数を使用することで、指定されたオプションに基づいて引数を解析することもできます。

```Gleam
let { "arg1", "arg2" } = command.parse_args(args, [flag("flag1"), string_flag("flag2")]) 
```

これらのオプションを使用することで、より柔軟にコマンドライン引数を読み取ることができます。

## See Also
- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [Gleamコマンドライン引数の解析](https://gleam.run/articles/command-line-arguments/)

コマンドライン引数を解析することで、プログラムをより柔軟にカスタマイズすることができるようになります。是非、この記事を参考にして、ご自身のプロジェクトでコマンドライン引数を活用してみてください。