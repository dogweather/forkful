---
title:                "現在の日付の取得"
aliases: - /ja/vba/getting-the-current-date.md
date:                  2024-02-01T21:54:57.069527-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
