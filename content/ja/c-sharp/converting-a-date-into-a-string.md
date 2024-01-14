---
title:                "C#: 日付を文字列に変換する"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ？
日付を文字列に変換する必要性は、プログラマーにとって実用的な理由があります。例えば、ユーザーに表示する日付フォーマットをコントロールすることができる、データベースに保存するための特定のフォーマットが必要な場合などが挙げられます。

## 方法
ほとんどの場合、C＃で日付を文字列に変換するには、`DateTime`オブジェクトの`ToString()`メソッドを使用します。以下のコードブロックに例を示します：

```C#
DateTime now = DateTime.Now;
string formattedDate = now.ToString("yyyy/MM/dd");
Console.WriteLine(formattedDate); // Output: "2021/11/30"
```

異なるフォーマットを使用することもできます。例えば、日付と時間の両方を含む場合は`"yyyy/MM/dd hh:mm:ss"`を使用します。また、カスタムフォーマットを使用することもできます。

## ディープダイブ
`ToString()`メソッドは「標準」のフォーマットパターンをサポートしています。しかし、より細かい制御ができるように、ユーザー定義のカスタムフォーマットパターンを使用することができます。

例えば、日付を`"MM/dd/yyyy"`の形式に変換したい場合は、次のようにします：

```C#
DateTime now = DateTime.Now;
string formattedDate = now.ToString("MM/dd/yyyy");
Console.WriteLine(formattedDate); // Output: "11/30/2021"
```

さらに、カスタムフォーマットパターンでは、日付の数値を制御するための特殊な文字も使用できます。例えば、`"ddd"`を使用すると、日付の曜日を表す短い文字列が表示されます。他の特殊な文字についての詳細は、Microsoftの公式ドキュメントを参照してください。

## さらに見る
- [DateTime.ToString() メソッドのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [カスタム日付と時刻の書式指定子の一覧](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)