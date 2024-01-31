---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:35:37.846890-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列から日付をパースするのは、文字列に含まれた日付情報をDateTimeオブジェクトに変換すること。データの保存形式が文字列だけど、日付として扱いたい場面でよく使われる。

## How to: (やり方)
C#では `DateTime.Parse` や `DateTime.TryParse` などのメソッドで日付パースが可能です。例を見てみましょう。

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023/04/01";
        
        // DateTime.Parse例
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate);  // "2023-04-01 00:00:00"
        
        // DateTime.TryParse例
        if(DateTime.TryParse(dateString, out DateTime tryParsedDate))
        {
            Console.WriteLine(tryParsedDate);  // "2023-04-01 00:00:00"
        }
        else
        {
            Console.WriteLine("パース失敗");
        }
        
        // カルチャを指定してパース
        CultureInfo provider = CultureInfo.InvariantCulture;
        string format = "yyyy/MM/dd";
        DateTime parsedDateWithCulture = DateTime.ParseExact(
            dateString, format, provider);
        Console.WriteLine(parsedDateWithCulture);  // "2023-04-01 00:00:00"
    }
}
```

## Deep Dive (深掘り)
日付のパースはC#の初期バージョンから存在します。`Parse`メソッドは適切な日付文字列の場合うまく動きますが、不正確なデータや不明な形式が来たときにはエラーを投げることがあります。

`TryParse`はこの問題に対処します。エラーを投げずに、パースできるかbool値で返してくれます。`ParseExact`および`TryParseExact`は特定の書式とカルチャを指定できるので、厳密な形式の制御が必要な場面で役立ちます。

他のライブラリもあります。`NodaTime`などは、より多機能で複雑な日付と時間操作を必要とするアプリケーション向けです。

パース処理は内部で`DateTimeFormatInfo`や`CultureInfo`を用いており、グローバル化されたアプリケーションではこれが重要になります。異なる地域の日付/時間形式は、アプリケーションを多言語に対応させる上で重要な役割を果たしています。

## See Also (参照)
- [DateTime.Parse メソッド (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parse)
- [DateTime.TryParse メソッド (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tryparse)
- [CultureInfo クラス (System.Globalization)](https://docs.microsoft.com/ja-jp/dotnet/api/system.globalization.cultureinfo)
- [NodaTime ライブラリ](https://nodatime.org/)
- [日付と時刻の書式設定](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/formatting-types#datetime-and-timespan-formatting)
