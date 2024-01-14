---
title:    "C#: 現在の日付の取得"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする人々にとって、現在の日付を取得することは非常に重要です。日付の情報は、多くのプログラムやアプリケーションで使用されており、正確な日付を取得することで、アプリケーションの機能やデータの整合性を確保することができます。

## 方法

日付を取得するには、C#で組み込みのDateTimeクラスを使用します。以下のようなコードを使用することで、現在の日付を取得することができます。

```C#
DateTime currentDate = DateTime.Now; // 現在の日付を取得
Console.WriteLine(currentDate); // 2021/12/16 というような形式で出力される
```

また、DateTimeクラスには、現在の年や月、曜日などの情報を個別に取得するための便利なメソッドも用意されています。例えば、以下のように使用することができます。

```C#
int currentYear = DateTime.Now.Year; // 現在の年を取得
Console.WriteLine(currentYear); // 2021 というような形式で出力される
```

## 深堀り

DateTimeクラスの詳細な使い方や、日付と時間を扱う方法については、公式ドキュメントを参照することをお勧めします。また、TimeZoneやCultureInfoを使用することで、異なる地域や言語に対応した日付を取得することもできます。

## 参考文献

- [Microsoft Docs: DateTime Structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [C# 超初心者向け入門！ 開発環境を整えて、最初の「Hello, world！」](https://www.atmarkit.co.jp/ait/articles/2101/27/news032.html)
- [C# 日時の扱いを覚えよう！ DateTime, TimeSpan, DateTimeOffSet の使い方まとめ](https://tech-blog.s-yoshiki.com/entry/2020/03/16/170000)

## 他の記事を見る

[もっと学びたい方へ！](https://www.atmarkit.co.jp/ait/articles/2101/27/news032.html) （今回の記事の続きとして、C#で日付を操作する方法について詳しく解説されています。）