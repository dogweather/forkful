---
date: 2024-01-20 17:55:30.677249-07:00
description: "How to: (\u65B9\u6CD5) \u304B\u3064\u3066DOS\u3084UNIX\u306E\u6642\u4EE3\
  \u306B\u306F\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u304C\u4E3B\
  \u306A\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3060\u3063\u305F\u3002\u73FE\
  \u4EE3\u3067\u306FGUI\u304C\u4E3B\u6D41\u3060\u3051\u3069\u3001\u5F15\u6570\u306F\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u3084\u81EA\u52D5\u5316\u306B\u306F\u4E0D\u53EF\u6B20\
  \u3002`string[]\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.017265-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u304B\u3064\u3066DOS\u3084UNIX\u306E\u6642\u4EE3\u306B\u306F\
  \u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u304C\u4E3B\u306A\u30A4\
  \u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3060\u3063\u305F\u3002\u73FE\u4EE3\u3067\
  \u306FGUI\u304C\u4E3B\u6D41\u3060\u3051\u3069\u3001\u5F15\u6570\u306F\u30B9\u30AF\
  \u30EA\u30D7\u30C8\u3084\u81EA\u52D5\u5316\u306B\u306F\u4E0D\u53EF\u6B20\u3002`string[]\
  \ args`\u306F`Main`\u30E1\u30BD\u30C3\u30C9\u306E\u5F15\u6570\u3068\u3057\u3066\u5229\
  \u7528\u3055\u308C\u3001\u305D\u3053\u306B\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  \u5F15\u6570\u304C\u683C\u7D0D\u3055\u308C\u308B\u3002`Environment.GetCommandLineArgs()`\u3092\
  \u4F7F\u3063\u3066\u53D6\u5F97\u3059\u308B\u624B\u3082\u3042\u308B\u304C\u3001`args`\u306E\
  \u65B9\u304C\u624B\u3063\u53D6\u308A\u65E9\u3044\u3002"
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
