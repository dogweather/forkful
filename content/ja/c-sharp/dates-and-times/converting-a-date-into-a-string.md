---
date: 2024-01-20 17:36:24.144987-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.012479-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\
  \u3059\u308B\u51E6\u7406\u306F`.ToString()`\u30E1\u30BD\u30C3\u30C9\u3067\u884C\u308F\
  \u308C\u307E\u3059\u3002`.NET`\u306E\u521D\u671F\u30D0\u30FC\u30B8\u30E7\u30F3\u304B\
  \u3089\u5229\u7528\u3067\u304D\u308B\u6A5F\u80FD\u3067\u3059\u3002\u3053\u306E\u30E1\
  \u30BD\u30C3\u30C9\u306F\u591A\u69D8\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u30D1\
  \u30BF\u30FC\u30F3\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3001`\"G\"`\u3001`\"D\"\
  `\u306A\u3069\u306E\u6A19\u6E96\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3001\u30AB\
  \u30B9\u30BF\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u6587\u5B57\u5217\u3092\u4F7F\
  \u7528\u3067\u304D\u307E\u3059\u3002\u307E\u305F\u3001`CultureInfo`\u30AF\u30E9\u30B9\
  \u3092\u4F7F\u3046\u3068\u5730\u57DF\u306B\u5408\u308F\u305B\u305F\u65E5\u4ED8\u8868\
  \u8A18\u3092\u751F\u6210\u3067\u304D\u308B\u305F\u3081\u3001\u56FD\u969B\u5316\u5BFE\
  \u5FDC\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u958B\u767A\u306B\u4FBF\
  \u5229\u3067\u3059\u3002\u4ED6\u306B\u540C\u3058\u76EE\u7684\u3067\u4F7F\u3048\u308B\
  \u65B9\u6CD5\u306B\u306F`String.Format()`\u3084`StringBuilder`\u30AF\u30E9\u30B9\
  \u304C\u3042\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u6587\u5B57\u5217\u64CD\u4F5C\
  \u304C\u6C42\u3081\u3089\u308C\u308B\u5834\u5408\u306B\u6D3B\u7528\u3067\u304D\u307E\
  \u3059\u3002`.NET 6`\u4EE5\u964D\u3067\u306F\u65E5\u4ED8\u3068\u6642\u9593\u306E\
  \u65B0\u305F\u306A\u30D1\u30BF\u30FC\u30F3\u3068\u3057\u3066DateOnly\u3068 TimeOnly\u578B\
  \u304C\u5C0E\u5165\u3055\u308C\u3066\u304A\u308A\u3001\u305D\u308C\u305E\u308C\u306E\
  \u60C5\u5831\u306E\u307F\u3092\u6271\u3048\u308B\u3088\u3046\u306B\u306A\u3063\u3066\
  \u3044\u307E\u3059\u3002"
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
