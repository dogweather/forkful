---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:55:30.677249-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むのは、プログラムが実行されるときにユーザーから追加情報を受け取る方法だ。なぜやるか？柔軟性を高め、同じプログラムで異なる結果を得られるようにするためだ。

## How to: (方法)
```C#
using System;

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

実行例:

```
> MyProgram.exe Hello World
Hello
World
```

## Deep Dive (深く掘り下げて)
かつてDOSやUNIXの時代には、コマンドライン引数が主なインターフェースだった。現代ではGUIが主流だけど、引数はスクリプトや自動化には不可欠。`string[] args`は`Main`メソッドの引数として利用され、そこにコマンドライン引数が格納される。`Environment.GetCommandLineArgs()`を使って取得する手もあるが、`args`の方が手っ取り早い。

## See Also (関連情報)
- [Microsoft C# Guide: Command-line arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Environment.GetCommandLineArgs Method](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs)
