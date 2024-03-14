---
date: 2024-01-20 17:55:30.677249-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u3055\u308C\u308B\
  \u3068\u304D\u306B\u30E6\u30FC\u30B6\u30FC\u304B\u3089\u8FFD\u52A0\u60C5\u5831\u3092\
  \u53D7\u3051\u53D6\u308B\u65B9\u6CD5\u3060\u3002\u306A\u305C\u3084\u308B\u304B\uFF1F\
  \u67D4\u8EDF\u6027\u3092\u9AD8\u3081\u3001\u540C\u3058\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u3067\u7570\u306A\u308B\u7D50\u679C\u3092\u5F97\u3089\u308C\u308B\u3088\u3046\u306B\
  \u3059\u308B\u305F\u3081\u3060\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.145811-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u3055\u308C\u308B\
  \u3068\u304D\u306B\u30E6\u30FC\u30B6\u30FC\u304B\u3089\u8FFD\u52A0\u60C5\u5831\u3092\
  \u53D7\u3051\u53D6\u308B\u65B9\u6CD5\u3060\u3002\u306A\u305C\u3084\u308B\u304B\uFF1F\
  \u67D4\u8EDF\u6027\u3092\u9AD8\u3081\u3001\u540C\u3058\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u3067\u7570\u306A\u308B\u7D50\u679C\u3092\u5F97\u3089\u308C\u308B\u3088\u3046\u306B\
  \u3059\u308B\u305F\u3081\u3060\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
