---
title:                "文字列の補間"
date:                  2024-02-25T17:06:54.053293-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-25, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
