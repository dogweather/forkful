---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:57.069527-07:00
description: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306F\u3001\u73FE\
  \u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u30DE\u30AF\u30ED\u3084\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3067\u65E5\u4ED8\u3092\u52D5\u7684\u306B\u6271\u3046\u3053\
  \u3068\u3092\u53EF\u80FD\u306B\u3059\u308B\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\
  \u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\
  \u30C8\u30E9\u30F3\u30B6\u30AF\u30B7\u30E7\u30F3\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\
  \u30F3\u30D4\u30F3\u30B0\u3001\u65E5\u4ED8\u30D9\u30FC\u30B9\u306E\u8A08\u7B97\u3092\
  \u884C\u3046\u969B\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:39.938995-07:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306F\u3001\u73FE\u5728\
  \u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u304C\u30DE\u30AF\u30ED\u3084\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3067\u65E5\u4ED8\u3092\u52D5\u7684\u306B\u6271\u3046\u3053\u3068\
  \u3092\u53EF\u80FD\u306B\u3059\u308B\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\
  \u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C8\
  \u30E9\u30F3\u30B6\u30AF\u30B7\u30E7\u30F3\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\
  \u30D4\u30F3\u30B0\u3001\u65E5\u4ED8\u30D9\u30FC\u30B9\u306E\u8A08\u7B97\u3092\u884C\
  \u3046\u969B\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications（VBA）では、現在の日付を取得することは、プログラマーがマクロやアプリケーションで日付を動的に扱うことを可能にする一般的なタスクです。この機能は、ログ記録、トランザクションのタイムスタンピング、日付ベースの計算を行う際に不可欠です。

## どのように：

VBAでは、`Date`関数を使用して現在の日付を取得するのは簡単ですが、`Now`関数は現在の日付と時刻の両方を提供します。両方を使用する方法は以下のとおりです：

```vb
Sub GetCurrentDate()
    ' Date関数を使用して現在の日付を取得
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Current Date: "; currentDate
    
    ' Now関数を使用して現在の日付と時刻を取得
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Current Date and Time: "; currentDateTime
End Sub
```

このマクロを実行すると、`Debug.Print`メソッドはVBAエディターの即時ウィンドウに現在の日付、そして現在の日付と時刻を出力します。例えば：

```
Current Date: 2023/04/12
Current Date and Time: 2023/04/12 15:45:22
```

日付形式は、ユーザーのコンピューターのシステム設定によって異なる可能性があることに注意してください。

## 深掘り

`Date`関数と`Now`関数は、Visual Basic for Applicationsで日付と時刻を扱う複雑さをカプセル化し、日付を扱う作業を簡単で直感的にするアプリケーションレベルの抽象化を提供します。歴史的に、日付と時刻をプログラミングで扱うことは、さまざまなタイムゾーン、夏時間の変更、異なる日付形式の扱いなど、困難でした。

VBAでは、これらの関数は基盤となるシステムの日付と時刻に依存するため、ユーザーのロケールとシステム設定の影響を受けます。これは、ユーザーの環境との一貫性を保証する一方で、グローバルアプリケーションでのローカリゼーションとタイムゾーンの調整を慎重に扱う必要があるという二重の刃です。

VBAの日付と時刻関数は、Officeの自動化の範囲内で特に、多くのアプリケーションに適していますが、高頻度取引システムや科学的シミュレーションなど、より複雑なアプリケーションには必要な精度や細かさが欠けている場合があります。そのような場合、PythonやC#などの他のプログラミング環境や言語が、より洗練された日付と時刻の操作ライブラリを提供するかもしれません。

それにもかかわらず、Excel、Word、または他のOfficeアプリケーションの文脈での日付と時刻を扱うタスクの大半に関して、VBAの`Date`関数と`Now`関数は、そのシンプルさ、パフォーマンス、および使用の容易さのバランスを提供し、打ち負かすのが難しいです。
