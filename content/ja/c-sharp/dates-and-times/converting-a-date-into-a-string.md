---
date: 2024-01-20 17:36:24.144987-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.139487-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (やり方)
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        DateTime now = DateTime.Now;
        
        // 標準の日付フォーマット
        string simpleDate = now.ToString("dd/MM/yyyy");
        Console.WriteLine(simpleDate); // 出力: 24/03/2023
        
        // 独自のフォーマット
        string customFormat = now.ToString("yyyy年MM月dd日 HH時mm分ss秒");
        Console.WriteLine(customFormat); // 出力: 2023年03月24日 13時45分18秒
        
        // カルチャを意識した日付フォーマット
        CultureInfo japaneseCulture = new CultureInfo("ja-JP");
        string japaneseDate = now.ToString("f", japaneseCulture);
        Console.WriteLine(japaneseDate); // 出力: 2023年3月24日 13:45
    }
}
```

## Deep Dive (深掘り)
日付を文字列に変換する処理は`.ToString()`メソッドで行われます。`.NET`の初期バージョンから利用できる機能です。このメソッドは多様なフォーマットパターンをサポートし、`"G"`、`"D"`などの標準フォーマットと、カスタムフォーマット文字列を使用できます。また、`CultureInfo`クラスを使うと地域に合わせた日付表記を生成できるため、国際化対応のアプリケーション開発に便利です。他に同じ目的で使える方法には`String.Format()`や`StringBuilder`クラスがあり、より複雑な文字列操作が求められる場合に活用できます。`.NET 6`以降では日付と時間の新たなパターンとしてDateOnlyと TimeOnly型が導入されており、それぞれの情報のみを扱えるようになっています。

## See Also (関連情報)
- [DateTime.ToString メソッド (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring)
- [標準の日付と時刻書式指定文字列 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [カスタム日付と時刻書式指定文字列 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo クラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.globalization.cultureinfo)
