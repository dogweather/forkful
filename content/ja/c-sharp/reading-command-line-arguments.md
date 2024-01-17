---
title:                "コンピュータプログラムに関する記事のタイトル：コマンドライン引数の読み取り"
html_title:           "C#: コンピュータプログラムに関する記事のタイトル：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラムに関する記事のタイトル：コマンドライン引数の読み取り"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## はじめに
プログラマーとして、コマンドライン引数を読み込むことが重要だと感じていますか？実際に、多くのプログラマーがコマンドライン引数を読み込んでプログラムをより柔軟にすることができます。それでは、C#を使用してコマンドライン引数を読み込む方法について見ていきましょう。

## 何か？
コマンドライン引数とは、コマンドラインツールやプログラムを実行する際に渡されるコマンドラインのパラメーターのことです。例えば、ファイル名やオプションなどがコマンドライン引数になります。

プログラマーがコマンドライン引数を読み込むのは、プログラムを実行する際に柔軟性を持たせるためです。ユーザーがプログラムを実行する際にさまざまなパラメーターを指定できるようにすることができるため、プログラムをより使いやすくすることができます。

## どのように？
C#では、Mainメソッドのパラメーターとしてstring配列を指定することでコマンドライン引数を読み込むことができます。以下の例では、ユーザーがプログラムを実行する際にパラメーターとして渡した文字列をコンソールに表示する方法を示します。

```C#
static void Main(string[] args)
{
    // 最初の引数を表示する
    Console.WriteLine(args[0]);
    
    // すべての引数を表示する
    foreach (var arg in args)
    {
        Console.WriteLine(arg);
    }
}

// プログラムを実行する際にパラメーターとして渡した文字列がコンソールに表示される
// > program.exe hello world
// hello
// hello
// world
```

## 詳細について
コマンドライン引数の読み込みは、C言語で開発されたUnixシステムから普及した機能です。現在では、ほぼすべてのプログラミング言語でサポートされています。

コマンドライン引数の代わりに、プログラム実行時にユーザーから対話的に入力を受け取る方法もあります。しかし、コマンドライン引数を使用することで、ユーザーが簡単にプログラムを実行できるだけでなく、自動化することもできます。

C#では、Environment.GetCommandLineArgsメソッドを使用することで、Mainメソッド以外でもコマンドライン引数を取得できます。また、CommandLineパッケージを使用することで、より柔軟なコマンドライン引数の読み込みが可能になります。

## 関連リンク
- [C#でコマンドライン引数を使用する方法 (Microsoft 公式ドキュメント)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [C# CommandLineパッケージ (NuGet Gallery)](https://www.nuget.org/packages/CommandLineParser/)