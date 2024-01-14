---
title:    "C#: 未来や過去の日付を計算する"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

日付計算は、特定の日付を指定し、その日付から未来または過去の日付を計算するために使用されます。たとえば、オンライン予約システムでは、ユーザーが将来の日付を指定して予約を確認できるように、日付計算が必要です。

## 方法

日付計算を実施するためには、DateTime構造体とTimeSpanクラスを使用します。まず、DateTimeオブジェクトを作成し、次に必要な日数をTimeSpanオブジェクトとして追加します。最後に、Addメソッドを使用して計算を実行し、結果を出力します。

```C#
//今日の日付を取得
DateTime today = DateTime.Today;

//2日後の日付を計算
TimeSpan addDays = new TimeSpan(2, 0, 0, 0);
DateTime futureDate = today.AddDays(addDays);

//結果を出力
Console.WriteLine("今日の日付: {0}", today.ToString("d"));
Console.WriteLine("2日後の日付: {0}", futureDate.ToString("d"));

/* 出力結果:
今日の日付: 2021/08/25
2日後の日付: 2021/08/27
*/
```

また、日付の引き算も同じように行うことができます。以下の例では、将来の日付から現在の日付を引いて、残りの日数を計算しています。

```C#
//将来の日付を取得
DateTime futureDate = new DateTime(2021, 09, 10);

//現在の日付を取得
DateTime currentDate = DateTime.Today;

//日付の引き算を実行
TimeSpan remainingDays = futureDate - currentDate; 

//残りの日数を出力
Console.WriteLine("残りの日数: {0}", remainingDays.Days);

/* 出力結果:
残りの日数: 16
*/
```

## ディープダイブ

日付計算には様々な方法があります。例えば、指定した日付の一年後や一ヶ月後を計算することもできます。また、DateTime構造体のプロパティやメソッドを使用して、曜日や年齢を計算することもできます。さらに、DateTimeOffsetクラスを使用すれば、タイムゾーンの考慮も行うことができます。

日付計算を行う際には、計算方法や扱うオブジェクトによって結果が異なることに注意しましょう。

## 参考リンク

- [Microsoft Docs: DateTime構造体 (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Microsoft Docs: TimeSpanクラス (System)](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)
- [日付計算を行うためのC#のTips | TechAcademyマガジン](https://techacademy.jp/magazine/19165)
- [C#における日付の記述方法についてのtipsまとめ | Qiita](https://qiita.com/rio-operation/items/4d73a5e0506c60751687) 

## もっと詳しく知りたい方へ

日付計算は、開発の様々な場面で必要になる機能です。より詳細な情報を知りたい方は、各リンク先のドキュメントや記事を参考にしてください。また、練習問題を解いて実際にコードを書くことで、より深