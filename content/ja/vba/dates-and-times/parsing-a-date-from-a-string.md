---
title:                "文字列から日付を解析する"
aliases: - /ja/vba/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:57:39.425868-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付を解析する"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications（VBA）で文字列から日付を解析するとは、日付を表すテキストを日付データ型に変換することです。プログラマーは、比較、計算、またはフォーマットの目的など、アプリケーションで日付をより効果的に扱うためにこれを行います。

## 方法：

VBAは、`CDate`関数または`DateValue`関数を使用して、文字列を日付に解析する直接的な方法を提供します。しかし、文字列が認識可能な日付形式であることが重要です。

ここに`CDate`を使用した基本的な例を紹介します：

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Parsed Date: "; parsedDate
End Sub
```

このコードを実行すると、VBAエディターで`Ctrl+G`を押すことでアクセシブルな即時ウィンドウに出力されるのは：

```
Parsed Date: 4/1/2023 
```

代わりに、時間部分を無視して日付に特化した`DateValue`関数を使用することもできます：

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Parsed Date using DateValue: "; parsedDate
End Sub
```

これに対するサンプル出力も同様に即時ウィンドウに表示されます：

```
Parsed Date using DateValue: 4/1/2023
```

文字列の日付形式がシステムまたはアプリケーションの設定と一致している必要があるため、解析の成功はそれに依存します。

## 深掘り

内部的に、VBAが文字列を日付に解析するとき、Windowsオペレーティングシステムの地域設定を使用して日付形式を解釈します。これは、異なる日時/時間設定を使用している場合、あるシステムで完璧に解析される日付文字列が別のシステムでエラーを引き起こす可能性があるため、理解することが重要です。

歴史的に、日付の処理は国際的に使用されるアプリケーションで一般的にバグの原因となっていました。VBAが地域設定に依存するこの理由は、異なるシステム間でのあいまいさのない日付表現と解析を可能にするISO 8601形式（例："YYYY-MM-DD"）などの代替手段を考慮する場合があるためです。残念ながら、VBAはネイティブにISO 8601をサポートしておらず、厳密な準拠のためには手動での解析が必要です。

`CDate`または`DateValue`で処理できるものを超える複雑な日付解析を行う場合、またはシステムのロケール設定に関係なく一貫した解析を保証するために、プログラマーはカスタム解析関数に頼ることがあります。これには、日付文字列をコンポーネント（年、月、日）に分割し、`DateSerial`関数を使用して日付を構築することが含まれる場合があります。また、そのようなタスクを想定した国際化を念頭に置いたより強力な言語やライブラリを選択する場合もあります。
