---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:39.425868-07:00
description: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u6587\u5B57\u5217\
  \u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\u3001\u65E5\u4ED8\
  \u3092\u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092\u65E5\u4ED8\u30C7\u30FC\u30BF\u578B\
  \u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u6BD4\u8F03\u3001\u8A08\u7B97\u3001\u307E\u305F\u306F\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u306E\u76EE\u7684\u306A\u3069\u3001\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u65E5\u4ED8\u3092\u3088\u308A\u52B9\u679C\u7684\
  \u306B\u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.900901-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u6587\u5B57\u5217\u304B\
  \u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\u3001\u65E5\u4ED8\u3092\
  \u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092\u65E5\u4ED8\u30C7\u30FC\u30BF\u578B\u306B\
  \u5909\u63DB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u6BD4\u8F03\u3001\u8A08\u7B97\u3001\u307E\u305F\u306F\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u306E\u76EE\u7684\u306A\u3069\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3067\u65E5\u4ED8\u3092\u3088\u308A\u52B9\u679C\u7684\u306B\
  \u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B"
weight: 30
---

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
