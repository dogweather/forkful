---
title:                "日付を文字列に変換する"
html_title:           "C#: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの利点は、日付から文字列を取得することができることです。例えば、データベースに格納された日付を人間が理解しやすい形式で表示する場合に、日付を文字列に変換することができます。

## 方法

日付を文字列に変換するためには、```ToString()``` メソッドを使用します。このメソッドは、指定した日付フォーマットに従って日付を文字列に変換してくれます。

例：今日の日付を文字列に変換するコード

```C#
DateTime today = DateTime.Today;
string strToday = today.ToString("yyyy/MM/dd");
Console.WriteLine(strToday);

// Output: 2021/08/26
```

## 深堀り

日付を文字列に変換する際、使用できる日付フォーマットには様々な種類があります。例えば、短い形式や長い形式、特定の言語や地域に合わせた形式などがあります。また、カスタムした形式を指定することも可能です。

さらに、日付をパースする際には、日付フォーマットの指定によってパースの成否が左右されることがあります。そのため、適切な日付フォーマットを指定することが重要です。

## 関連リンク

[TryParseExact メソッド (System.DateTime)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tryparseexact?view=net-5.0)

[日付の書式の指定 (C# リファレンス)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/custom-date-and-time-format-strings)