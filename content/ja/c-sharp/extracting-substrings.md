---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列抽出は、元の文字列から特定の部分文字列を切り出すことを指します。プログラマーは、取り扱うことの多いテキスト操作でもあり、データの検索や特定の情報の取り出しを簡単にするためにタスクを行います。

## 方法：

C#では、Substringメソッドを使用して文字列から部分文字列を抽出します。次はその例です：

```C#
string str = "Hello, World!";
string substr = str.Substring(0, 5);
Console.WriteLine(substr);
```
出力：
```
Hello
```
この例では、文字列から最初の5文字を抽出しています。Substringメソッドの第一引数は開始インデックスで、第二引数は抽出する文字数です。

## Deep Dive:

過去には、別の方法で部分文字列を抽出することもありました。特に古いプログラミング環境では、ArrayやListのようなデータ構造とインデックスを使って部分文字列を抽出することがよく行われました。しかし、C#のような現代的な言語では、この操作がSubstringメソッドのように組み込まれています。

部分文字列の抽出は、元の文字列を変更せずに新しい文字列を作成するため、メモリに影響を与えます。しかし、必要なケースでは他の手段を超えるほど便利で効率的な操作です。

また、Substringメソッドの代わりにSpan<T>やMemory<T>を使用するという選択肢もあります。これらはメモリ効率を向上させる可能性がありますが、使い勝手は少し違います。

## 参照：

- マイクロソフト公式ドキュメンテーション：[String.Substring メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.substring?view=net-5.0)
- [Span<T>について詳しく](https://docs.microsoft.com/ja-jp/dotnet/api/system.span-1?view=net-5.0)
- [Memory<T>について詳しく](https://docs.microsoft.com/ja-jp/dotnet/api/system.memory-1?view=net-5.0)