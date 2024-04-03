---
date: 2024-01-20 17:55:30.677249-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.145811-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
