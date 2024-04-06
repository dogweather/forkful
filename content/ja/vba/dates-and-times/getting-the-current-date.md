---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:57.069527-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A VBA\u3067\u306F\u3001`Date`\u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\
  \u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u304C\u3001`Now`\u95A2\u6570\u306F\
  \u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u306E\u4E21\u65B9\u3092\u63D0\u4F9B\
  \u3057\u307E\u3059\u3002\u4E21\u65B9\u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\
  \u4EE5\u4E0B\u306E\u3068\u304A\u308A\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.797679-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
