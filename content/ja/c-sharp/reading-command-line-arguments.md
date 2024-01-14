---
title:    "C#: コマンドライン引数の読み取り"
keywords: ["C#"]
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読むのか
プログラミングにおいて、コマンドライン引数を読むことは非常に便利です。これにより、ユーザーがプログラムに特定の値を渡すことができます。例えば、ファイル名やフラグなどの変数値を渡すことで、プログラムをより柔軟に使うことができます。

## コマンドライン引数の読み方
`Main`メソッド内で、`args`パラメーターを使用することでコマンドライン引数を読むことができます。以下のコードブロックに例を示します。

```C#
static void Main(string[] args)
{
    // コマンドライン引数の数を表示
    Console.WriteLine("コマンドライン引数の数：" + args.Length);

    // コマンドライン引数の順番で値を表示
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("コマンドライン引数" + (i + 1) + "：" + args[i]);
    }
}
```

実行結果は以下のようになります。

```
コマンドライン引数の数：3
コマンドライン引数1：file.txt
コマンドライン引数2：-d
コマンドライン引数3：output.txt
```

## コマンドライン引数の深い掘り下げ
コマンドライン引数を使用することで、プログラムに様々な機能や振る舞いを与えることができます。また、`args`パラメーターには`string`型の配列としてコマンドライン引数の値が渡されますが、それ以外にも`Environment.GetCommandLineArgs()`メソッドを使用することで、プログラムが実行された際に受け取った全てのコマンドライン引数を取得することができます。

さらに、コマンドライン引数には文字列以外の値も与えることができます。例えば、`int`型や`bool`型、さらにはカスタムのオブジェクト型なども渡すことができます。ただし、コマンドライン引数は文字列として渡されるため、値を適切な型に変換する必要があります。

# See Also
- [C# ガイド – コマンド ライン 引数(英語)](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Environment.GetCommandLineArgs メソッド (System) (英語)](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs)
- [コマンド ライン 引数を含めてプログラムを実行する (英語)](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-run)