---
title:    "C#: コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##なぜコマンドライン引数を読むのか

コマンドライン引数を読むことは、プログラマーにとって非常に重要なスキルです。コマンドライン引数を使うことで、プログラムを実行する際にさまざまなオプションやパラメーターを指定することができます。これにより、より柔軟性のあるプログラムを作成することができます。

##方法

コマンドライン引数を読み込むための基本的な方法は、`Main`メソッドの引数を使用することです。以下のように、`string[] args`を`Main`メソッドの引数として定義します。

```C#
static void Main(string[] args)
{
    //コマンドライン引数を入力するためのコード
}
```

次に、コマンドライン引数を読み取るための基本的なコード例を挙げます。

```C#
static void Main(string[] args)
{
    //コマンドライン引数を読み込む
    //引数が存在するかどうかを確認する
    if (args.Length > 0)
    {
        //最初の引数を取得する
        string firstArg = args[0];
        Console.WriteLine("最初の引数は " + firstArg + " です。");
    }

    //すべての引数をループして表示する
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```

上記のコードを実行すると、コマンドラインから入力された引数が表示されます。例えば、`dotnet run arg1 arg2 arg3`というコマンドを実行すると、次のような出力になります。

```console
最初の引数は arg1 です。
arg1
arg2
arg3
```

##ディープダイブ

コマンドライン引数を読み込む際に注意するべき点として、引数の数や型を確認することが挙げられます。引数の数が定義したよりも多い場合や、引数の型が期待されるものと異なる場合にはエラーが発生する可能性があります。また、デフォルト値を設定することで、引数が省略された場合にエラーを回避することができます。

さらに、コマンドライン引数を使用することで、プログラムの実行時に動的に処理を変更することができます。例えば、引数を使用して特定のファイルを読み込み、処理することができます。また、より高度な方法として、コマンドライン引数を使ってオプションを設定することができます。これにより、ユーザーがより簡単にプログラムをカスタマイズすることができます。

##参考リンク

- [C#コマンドライン引数](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# でコマンドライン引数を解析する](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-argument-parsing)
- [C# でのデフォルト引数](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/classes-and-structs/named-and-optional-arguments)
- [コマンドライン引数を使いこなすためのTips](https://www.toptal.com/developers