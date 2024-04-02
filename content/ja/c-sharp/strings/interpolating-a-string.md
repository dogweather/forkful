---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:54.053293-07:00
description: "C#\u3067\u306E\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u6587\u5B57\
  \u5217\u30EA\u30C6\u30E9\u30EB\u5185\u306B\u5F0F\u3092\u542B\u3081\u308B\u3053\u3068\
  \u3067\u3001\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u6587\u5B57\
  \u5217\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u9023\u7D50\u3092\u5BB9\u6613\
  \u306B\u3059\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52D5\
  \u7684\u306A\u6587\u5B57\u5217\u5185\u5BB9\u3092\u6271\u3046\u969B\u306B\u30B3\u30FC\
  \u30C9\u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\
  \u3055\u305B\u308B\u305F\u3081\u306B\u3053\u306E\u6A5F\u80FD\u3092\u5229\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.105655-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u306E\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u6587\u5B57\u5217\
  \u30EA\u30C6\u30E9\u30EB\u5185\u306B\u5F0F\u3092\u542B\u3081\u308B\u3053\u3068\u3067\
  \u3001\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\
  \u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u6587\u5B57\u5217\
  \u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u9023\u7D50\u3092\u5BB9\u6613\u306B\
  \u3059\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u52D5\u7684\
  \u306A\u6587\u5B57\u5217\u5185\u5BB9\u3092\u6271\u3046\u969B\u306B\u30B3\u30FC\u30C9\
  \u306E\u8AAD\u307F\u3084\u3059\u3055\u3068\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u306E\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## 何となく理由
C#での文字列補間は、文字列リテラル内に式を含めることで、新しい文字列を作成することを可能にします。これは文字列のフォーマットと連結を容易にするため、プログラマーは動的な文字列内容を扱う際にコードの読みやすさと保守性を向上させるためにこの機能を利用します。

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
