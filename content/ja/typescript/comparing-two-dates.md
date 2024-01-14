---
title:                "TypeScript: 「2つの日付を比較する」"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日常生活やビジネスの中で、時期や期限を把握することは非常に重要です。そのため、プログラマーは時系列のデータを扱う際に、日時を比較する必要があります。それでは、TypeScriptを使用して日時の比較を行う方法を紹介します。

## 方法
日付を比較するには、Dateオブジェクトを使います。まず最初に、比較したい2つの日付をそれぞれDateオブジェクトに変換します。次に、以下のようなコードを使用して日付の比較を行います。

```TypeScript
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2020-12-31");

if (date1 > date2) {
  console.log("date1の方が未来の日付です");
} else if (date1 < date2) {
  console.log("date2の方が未来の日付です");
} else {
  console.log("両方の日付が同じです");
}
```

上記のように、Dateオブジェクト同士を比較することで、日付の大小や等しさを判定することができます。また、年、月、日、時、分、秒など、より詳細な情報を比較することも可能です。詳細なコードの例は、[こちら](https://www.typescriptlang.org/play?#code/PTAFwKgAQhgcgYygFgBQEMBPgVwAUBXZAI3aa2aBTAQwBdUO6QG8A4SAzgCNGMLADZACxCMAmiqdwBjRwDCAMxBKvAAxnNAGWNNYABcA5JZAVy20j7joAoAPQDm5LJAO6ZbhfXZG0eeAE5igJNWqaG4NnaAB8qO3lAJ4rdhhAWeBkYkGRkZmZg4dGZ3dXQIAwAslPCYoPXyC5qh CEQNOKkXCesNkMXd+7ALZAZQkKgei4Hzk2ZFBjHkZmDQ871s7rDj4WTWWb26bITgDOZRgACQgkCxCEALw+CdJAK1yABiIUAizgDAEQhBhBS5CEYaFgIgUAhxg0Ai koCkpu+Cg5YQgNAFUw-AEpRbly8iGJAEYXlRMLGPMMIACHD4VpeAJlBhQAEZcJAcAAgAv5MGHKmDhWqkZgAewBXJWFpMIClS1Nx2KrKAA)でご確認いただけます。

## ディープダイブ
さらに、Dateオブジェクトを使用してより複雑な比較を行うことも可能です。例えば、ある日付が特定の範囲内に収まるかどうか、ある日付が何日後・前であるかなど、様々な条件を比較することができます。詳細なコードの例は、[こちら](https://www.typescriptlang.org/play?#code/JYOwLgpgTgZghgYwgAgGIHtUB2GBcWAdgKJYDWEAUFKAnqQL4AXgwFZpLUGoHGAEwALKZbDIAGYKgC+HCD2MIA8MR8AmysoAXUWzFAGYIEADgBSToYIAlEVAL7QSwJygKJUaVGkg8AmlC3mlPy2bEF83CwZ-Rw3JlLA29JQAL4bl5