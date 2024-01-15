---
title:                "デバッグ出力の印刷"
html_title:           "C#: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を行う理由は、コードの実行中に起きている問題やバグを特定し、修正するためです。

## 方法

デバッグ出力を行うには、`Console.WriteLine()`メソッドを使用します。以下のコードは、変数`num`の値を出力する例です。

```C#
int num = 10;
Console.WriteLine("numの値は" + num + "です。");
```
実行結果は、`numの値は10です。`となります。

また、配列やオブジェクトの内容を出力することもできます。以下のコードは、配列`numbers`の要素を出力する例です。

```C#
int[] numbers = { 1, 2, 3, 4, 5 };
Console.WriteLine("numbersの要素は" + string.Join("、", numbers) + "です。");
```
実行結果は、`numbersの要素は1、2、3、4、5です。`となります。

## 深堀り

デバッグ出力を行う際には、`Console.WriteLine()`メソッドの代わりに`Debug.WriteLine()`メソッドを使用することもできます。このメソッドを使用すると、コンパイル時にデバッグ出力が無視されるようになります。

また、特定の条件下でのみデバッグ出力を行いたい場合は、`if`文を使用して条件を設定することもできます。例えば以下のように、変数`debugMode`が`true`の場合のみデバッグ出力を行うように設定することができます。

```C#
if(debugMode)
{
    Console.WriteLine("デバッグメッセージを出力します。");
}
```

## 参考リンク

- [Console.WriteLine()の使用方法](https://docs.microsoft.com/ja-jp/dotnet/api/system.console.writeline)
- [Debug.WriteLine()の使用方法](https://docs.microsoft.com/ja-jp/dotnet/api/system.diagnostics.debug.writeline)