---
title:    "C#: 標準エラーへの書き込み"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
標準エラーに書き込むことに参加する理由は、バグやエラーの修正に役立つことです。

## 方法
以下のように、C#コードブロック内にコーディングの例とサンプル出力を示します。
```C# 
 using System;

class Program {
  static void Main() {
    Console.Error.WriteLine("エラーが発生しました。");
  }
}
```

## 深堀り
標準エラーに書き込むことの利点は、デバッグ時に役立つことだけではありません。エラーログを作成することで、アプリケーションの健全性や把握度を向上させることもできます。また、ユーザーにとってもエラーが発生したことを伝えることができ、よりスムーズなユーザーエクスペリエンスを提供することができます。

## さらに見る
- [C#のエラーハンドリングの仕組みについて](https://www.w3schools.com/cs/cs_exceptions.asp)
- [標準エラーと標準出力の違いを理解する](https://stackoverflow.com/questions/2769926/what-are-the-differences-between-stdio-and-stderr)
- [エラーログの重要性についての記事](https://www.splunk.com/ja_jp/blog/devops/error-logging-champions-ensure-system-reliability.html)

## 参考
[C#標準エラーについての公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.console.error)