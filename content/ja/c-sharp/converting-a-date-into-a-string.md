---
title:    "C#: 日付を文字列に変換する"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

日付を文字列に変換する理由は何でしょうか？日付を処理する際、時には文字列として扱う必要があります。たとえば、データベース内の日付を文字列として表示したり、ファイル名として日付を使用したりする場合です。C＃では、日付を文字列に変換するための簡単で効率的な方法が提供されています。

## How To

C＃で日付を文字列に変換するには、まずDateTimeオブジェクトを作成する必要があります。次に、ToStringメソッドを使用して日付を文字列に変換します。このメソッドには、文字列として表示する形式を指定することができます。例えば、次のコードを使用すると、"yyyy年MM月dd日"という形式で日付を表示することができます。

```C# 
DateTime date = new DateTime(2020, 12, 31);
string dateString = date.ToString("yyyy年MM月dd日");
Console.WriteLine(dateString);
```

このコードの出力は、"2020年12月31日"となります。また、ToStringメソッドには、カルチャー情報を指定するオーバーロードもあります。これにより、異なる地域の日付表記に対応することができます。例えば、英語圏のコンピューターでは"MM/dd/yyyy"という形式が一般的ですが、日本語圏のコンピューターでは"yyyy/MM/dd"という形式が一般的です。

## Deep Dive

日付を文字列に変換する際、一定のルールに従う必要があります。これには、時間、曜日、月の数値・名前など、特定の表現方法が決められています。C＃では、カスタムフォーマット指定子を使用して、日付を任意の形式で表示することも可能です。また、.NET Frameworkでは、String.Formatメソッドを使用することで、日付を文字列に変換することができます。

日付を文字列に変換する際、重要なのは日付と文字列の相互変換を正しく行うことです。また、日付がフォーマット指定子に準拠しない場合、例外が発生することもあります。このため、厳密なエラー処理や例外処理が必要になることもあります。

## See Also

- [DateTime.ToString メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring?view=net-5.0#System_DateTime_ToString_System_String_)
- [String.Format メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.format?view=net-5.0)
- [カルチャー指定子を使用した日付と時刻の表現方法](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/custom-date-and-time-format-strings#culture-specifiers)