---
title:                "コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
html_title:           "C#: コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
simple_title:         "コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ？

コマンドライン引数を読み取る方法を学ぶことで、C#コードをより動的かつ効率的に作成することができます。また、ユーザーからの入力を受け取るための重要なスキルです。

## 方法

まず、```Main()```メソッドの引数に```string[] args```を追加します。これにより、コマンドライン引数が文字列の配列として受け取られます。

次に、```args```配列の各要素を```foreach```ループで処理することができます。例えば、以下のようにコマンドライン引数を出力することができます。

```C#
foreach (string arg in args)
{
    Console.WriteLine(arg);
}
```

入力と同じ順序で引数が出力されることがわかります。

## 詳細

コマンドライン引数を読み取る際には、引数が存在するかどうかを確認することが重要です。```args.Length```を使用することで、引数の数を取得することができます。

また、引数の数が不正な場合、検証する必要があります。例えば、引数の数が足りない場合には、プログラムを中断するようにすることができます。

さらに、```args```配列の要素は文字列として受け取られますが、必要に応じて型変換を行うことも可能です。例えば、```Int32.Parse()```メソッドを使用することで、引数を整数として扱うことができます。

## 関連情報

[コマンドライン引数のサポート](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)について詳しくは、Microsoftの公式ドキュメントを参照してください。

見てみると、この記事でも同じくらいの長さで、丁寧な説明がされていますね。参考にしてみてください。