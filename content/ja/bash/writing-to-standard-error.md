---
title:                "「標準エラーに書く」"
html_title:           "Bash: 「標準エラーに書く」"
simple_title:         "「標準エラーに書く」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Bashは、LinuxやmacOSなど主要なオペレーティングシステムで使用されるコマンドラインインターフェイスです。エラーが発生した場合、Bashでは標準エラー出力を使用してエラーメッセージを表示することができます。この機能は、プログラムの実行時に問題が発生した場合に、ユーザーにエラーを通知するために役立ちます。

## How To
Bashで標準エラー出力を使用するには、`>&2`をコマンドの最後に追加します。例えば、`echo "Hello" >&2`と書くと、ターミナルに`Hello`というメッセージが表示されます。ただし、エラーが発生していない場合でも、エラーメッセージが表示されることに注意してください。

```Bash
$ echo "Hello" >&2
Hello
```

エラーを発生させるために`ls`コマンドを使用します。`ls`コマンドは、存在しないファイルを指定するとエラーを返します。今度は、エラーメッセージが標準エラー出力に表示されることがわかります。

```Bash
$ ls invalid_file >&2
ls: invalid_file: No such file or directory
```

また、標準エラー出力をリダイレクトして、エラーメッセージをファイルに保存することもできます。以下の例では、`invalid_file`というファイルが見つからないエラーメッセージが`error.log`というファイルに保存されます。

```Bash
$ ls invalid_file 2>error.log
$ cat error.log
ls: invalid_file: No such file or directory
```

## Deep Dive
Bashでは、標準エラー出力を扱うためのいくつかの特殊なファイル記述子を使用します。上記の例では、`2`というファイル記述子を使用して標準エラー出力をリダイレクトしています。また、`1`というファイル記述子を使用することで、標準出力をリダイレクトすることもできます。

標準エラー出力は、通常の標準出力（`1`）とは別のストリームとして扱われます。このため、標準出力と標準エラー出力を同時にリダイレクトするには、以下のように書きます。

```Bash
$ ls invalid_file >output.log 2>&1
```

このコマンドでは、標準出力を`output.log`ファイルにリダイレクトし、さらに`2>&1`を使用して標準エラー出力を標準出力にリダイレクトします。つまり、エラーメッセージも`output.log`ファイルに含まれることになります。

## See Also
- [リダイレクトとパイプの基礎](https://qiita.com/kasei-san/items/d1a284344e411d5507cd)
- [Bashの基本コマンド集](https://qiita.com/take4mats/items/605d8327eb2e2325f9e8)