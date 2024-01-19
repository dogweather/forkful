---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何それ & なぜ？ (What & Why?)
コマンドライン引数とは、プログラムに渡す外部入力のことです。プログラマーはこれを使用して、プログラムの動作を制御または変更します。

## 実行方法 (How to:)
```C#
class Program
{
    static void Main(string[] args)
    {
        foreach (var arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```
コマンドラインから次のように実行します。`program.exe arg1 arg2 arg3`
出力は次の入力に反映されます。
```C#
arg1
arg2
arg3
```

## じっくりと調べる (Deep Dive)
コマンドライン引数の利用は、古くから存在する技術で、一部のプログラムではユーザー入力の主要な形態となっています。選択肢としては、環境変数や設定ファイルなどの他の入力源を利用することも可能ですが、コマンドライン引数は直感的で簡単です。C#では、string型の配列にすべての引数が格納されます。これらの引数は実行ファイルの名前の後にスペースで分けて記述されます。

## 参考文献 (See Also)
- [コマンドライン引数についてさらに知る (Learn more about command line arguments)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [他の入力方法について学ぶ (Learn about other input methods)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [C# の詳細なチュートリアル (Detailed C# tutorials)](https://www.microsoft.com/ja-jp/dev/tutorial/csharp)