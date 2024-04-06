---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:54.053293-07:00
description: "\u65B9\u6CD5 C#\u3067\u306E\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\
  \u30C9\u30EB\u8A18\u53F7(`$`)\u306B\u7D9A\u3044\u3066\u6587\u5B57\u5217\u30EA\u30C6\
  \u30E9\u30EB\u306B\u3088\u3063\u3066\u793A\u3055\u308C\u307E\u3059\u3002\u5909\u6570\
  \u540D\u3084\u5F0F\u306F\u4E2D\u62EC\u5F27(`{}`)\u3067\u56F2\u307E\u308C\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T21:53:42.982145-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## 方法
C#での文字列補間は、ドル記号(`$`)に続いて文字列リテラルによって示されます。変数名や式は中括弧(`{}`)で囲まれます。

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"こんにちは、{name}さん！ あなたは{age}歳です。";
Console.WriteLine(interpolatedString);
// 出力: こんにちは、Janeさん！ あなたは28歳です。
```

もう少し複雑な例では、中括弧内で操作を実行したりメソッドを呼び出したりすることができます:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"合計価格: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// 出力: 合計価格: $59.97
```
中括弧内の`:C2`形式指定子は、数値を2小数点の通貨としてフォーマットします。

より高度なフォーマットやローカリゼーションが必要な場合は、`string.Format`メソッドやHumanizerのようなライブラリの使用を検討するかもしれません。Humanizerは、文字列、日付、時間、タイムスパン、数値、量をより人間が読みやすい形式で操作し表示することができます。以下は、Humanizerを使用して複雑な文字列操作を行う例です。Humanizerは.NET標準ライブラリの一部ではなく、NuGetパッケージ`Humanizer`をインストールする必要があります。

まず、NuGet経由でHumanizerをインストールします:

```
Install-Package Humanizer
```

その後、以下のように使用します:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"そのイベントは{dayDifference}日前でした。".Humanize();
Console.WriteLine(humanized);
// 設定とカルチャーによって、可能な出力: そのイベントは5日前でした。
```

この例は基本的な使い方を示しています。Humanizerは、文字列、日付、数値などに適用できる幅広い機能をサポートしており、アプリケーションをよりアクセスしやすく直感的にします。
