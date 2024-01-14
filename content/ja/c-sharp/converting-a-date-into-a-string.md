---
title:                "C#: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由

日付を使用するプログラムで、必要に応じて日付をテキストに変換することがあります。 これは、ユーザーが見やすい形式に日付を表示するためだけでなく、データベースや他のソフトウェアとの互換性も高めることができます。 

## 方法
日付を文字列に変換する方法を示すコーディング例とサンプル出力

```C#
// 日付を文字列に変換する方法
// DateTimeオブジェクトを作成
DateTime date = new DateTime(2020, 1, 1);
// 文字列に変換
string dateString = date.ToString("yyyy/MM/dd");
// 出力: 2020/01/01
```

上記の例では、DateTimeオブジェクトを作成し、ToStringメソッドを使用して任意の形式で日付を文字列に変換しています。 ToStringメソッドの引数には、日付の表示形式を指定するカスタムフォーマット文字列を渡すことができます。 "yyyy/MM/dd"というフォーマット文字列では、年、月、日の順に表示されることを示しています。 その他のカスタムフォーマットの例や、日付を文字列に変換する際に注意すべきポイントなど、詳細な情報は下記のリンクを参考にしてください。

## 深堀り
日付を文字列に変換する際のさまざまな方法や注意点についての詳細説明

日付を文字列に変換する際には、上記の例のようにToStringメソッドを使用するほかにも、String.FormatやStringBuilderクラスなどを使用する方法があります。 また、日付のフォーマット文字列には、"yyyy/MM/dd"以外にも様々な形式があり、曜日や時間などの情報も含めて出力することができます。 さらに、日付を文字列に変換する際にはロケール（地域や言語）の設定にも注意が必要です。 日本語の環境では、例えば曜日を表す"ddd"というフォーマット文字列は日本語で曜日を表示することができますが、英語の環境では曜日は英語で表示されるため、注意が必要です。

## 参考リンク
- [DateTime.ToString メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring?view=netcore-3.1)
- [日付のカスタムフォーマット指定 - .NET入門をプログラミング初心者のために網羅したリファレンス](https://www.atmarkit.co.jp/ait/articles/0212/07/news012.html)
- [日時の表示フォーマット - .NET入門をプログラミング初心者のために網羅したリファレンス](https://www.atmarkit.co.jp/ait/articles/0211/22/news011.html)