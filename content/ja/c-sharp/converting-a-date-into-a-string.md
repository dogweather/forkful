---
title:    "C#: 日付を文字列に変換する"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由について、皆さん考えたことはありますか？日付を文字列に変換することで、プログラム内で日付をより簡単に操作することができるようになります。具体的にどのようにして日付を文字列に変換するか、ご紹介します。

## 方法

日付を文字列に変換するためには、一番簡単な方法は文字列フォーマットを使用することです。下記のコード例を参考にしてみてください。

```C#
var date = DateTime.Today;
string dateString = date.ToString("MM/dd/yyyy"); // "07/16/2021"という文字列が生成されます
```

このように、DateTimeオブジェクトをToStringメソッドによって文字列に変換することができます。もちろん、文字列フォーマットは様々な形式を指定することができます。例えば、"yyyy-MM-dd"のようにすると"2021-07-16"というように年月日の順番に並び替えることができます。

また、もし日付を特定の言語に変換したい場合は、CultureInfoクラスを使用することで可能です。下記のコード例を参考にしてみてください。

```C#
var date = DateTime.Now;
string dateString = date.ToString("M", new CultureInfo("ja-JP")); // 日本語の月を表現する文字列に変換します
```

## ディープダイブ

日付を文字列に変換する際に、もう少し詳しく説明します。まずDateTimeオブジェクトはToStringメソッドによって文字列に変換される前に、DateType構造体によって日付と時刻の値が保持されています。ToStringメソッドは指定された書式に従って、必要な値を抽出して文字列として返します。そのため、どのように書式指定を行うかによって、最終的な文字列の形式が決まります。

さらに、DateTimeオブジェクトは様々なメソッドを持っており、日付や時刻の計算なども可能です。状況に応じてDateTimeオブジェクトをうまく活用して、日付を正しく操作できるようにしましょう。

## その他の参考リンク

- [DateTime.ToString メソッド (System) | Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.datetime.tostring?view=net-5.0)
- [DateTime構造体 (System) | Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.datetime?view=net-5.0)
- [CultureInfoクラス (System.Globalization) | Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.globalization.cultureinfo?view=net-5.0)

## そのほかの記事

[date-time-conversion.md](https://github.com/exampleuser/date-time-conversion)