---
date: 2024-01-20 17:36:24.144987-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u306F\u3001DateTime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u30C6\u30AD\
  \u30B9\u30C8\u3068\u3057\u3066\u8868\u793A\u307E\u305F\u306F\u4FDD\u5B58\u3057\u3084\
  \u3059\u304F\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u3001UI\u3067\u306E\u8868\u793A\u3001\u30ED\u30B0\
  \u30D5\u30A1\u30A4\u30EB\u3078\u306E\u65E5\u4ED8\u306E\u8A18\u9332\u3001\u30C7\u30FC\
  \u30BF\u30D9\u30FC\u30B9\u3068\u306E\u4E92\u63DB\u6027\u3092\u4FDD\u3064\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.286960
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u306F\u3001DateTime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u30C6\u30AD\
  \u30B9\u30C8\u3068\u3057\u3066\u8868\u793A\u307E\u305F\u306F\u4FDD\u5B58\u3057\u3084\
  \u3059\u304F\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u3001UI\u3067\u306E\u8868\u793A\u3001\u30ED\u30B0\
  \u30D5\u30A1\u30A4\u30EB\u3078\u306E\u65E5\u4ED8\u306E\u8A18\u9332\u3001\u30C7\u30FC\
  \u30BF\u30D9\u30FC\u30B9\u3068\u306E\u4E92\u63DB\u6027\u3092\u4FDD\u3064\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換することは、DateTimeオブジェクトをテキストとして表示または保存しやすくする方法です。プログラマーはこれを、UIでの表示、ログファイルへの日付の記録、データベースとの互換性を保つために行います。

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
