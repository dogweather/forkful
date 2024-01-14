---
title:                "C#: コンピュータプログラミングの記事タイトル：「コマンドライン引数の読み込み」"
simple_title:         "コンピュータプログラミングの記事タイトル：「コマンドライン引数の読み込み」"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読むのか

コマンドライン引数を読むことは、プログラマーにとって非常に重要なスキルです。コマンドライン引数を読むことにより、ユーザーがプログラムを起動する際にコマンドラインから入力した値を取得することができます。これにより、ユーザーがプログラムをより柔軟に操作できるようになります。さらに、コマンドライン引数を取得することで、プログラムの実行中に必要な情報を追加することもできます。

## 使い方

コマンドライン引数を読むには、複数の方法があります。まず最初に、プログラムの実行時に指定された全ての引数を取得する方法です。以下の例を参考にしてください。

```C#
using System;

class Program 
{
  static void Main(string[] args) 
  {
    Console.WriteLine("指定された引数の数は: {0}", args.Length);
    for (int i = 0; i < args.Length; i++) 
    {
      Console.WriteLine(args[i]);
    }
  }
}
```
このプログラムを実行すると、コマンドラインから入力した引数の数とその値が表示されます。

実行時に引数を指定しなかった場合、`args.Length`は0になります。そのため、引数がない場合の処理も行う必要があります。

もう一つの方法は、指定した引数を個別に取得することです。以下の例を参考にしてください。

```C#
using System;

class Program 
{
  static void Main(string[] args) 
  {
    string name = args[0]; // 最初の引数を取得
    int age = int.Parse(args[1]); // 二番目の引数を数値に変換して取得
    Console.WriteLine("私の名前は{0}です。年齢は{1}歳です。", name, age);
  }
}
```

このプログラムを実行する際には、引数として名前と年齢を順番に入力する必要があります。

## 詳細を深く掘る

コマンドライン引数の取得については、さらに詳しい情報があります。例えば、引数をオプションとして設定したり、引数にデフォルト値を設定したりすることもできます。また、引数の値をチェックしてエラーを防ぐ方法もあります。これらの情報については、[この記事](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)を参考にしてください。

コマンドライン引数は、プログラムをより柔軟に制御するために欠かせないものです。ぜひ覚えておいて、自分のプログラムに活用してみてください。

## 関連情報を見る

- [C#ドキュメント: Main メソッドとコマンドライン引数](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C#ドキュメント: コマンドライン引数を使用する](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/how-to-use-command-line-arguments)
- [C#ドキュメント: コマンドライン引数を正しく処理する](https://docs.microsoft.com/ja-jp/archive/blogs/kebab/executing-a-command-line-tool-sensibly)