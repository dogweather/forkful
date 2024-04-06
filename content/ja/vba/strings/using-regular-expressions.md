---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:28.884888-07:00
description: "\u4F7F\u3044\u65B9\uFF1A VBA\u3067\u6B63\u898F\u8868\u73FE\u3092\u4F7F\
  \u7528\u3059\u308B\u306B\u306F\u3001\u6700\u521D\u306B Microsoft VBScript Regular\
  \ Expressions \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6709\u52B9\u306B\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002VBA\u30A8\u30C7\u30A3\u30BF\u3067\
  \ `\u30C4\u30FC\u30EB` -> `\u53C2\u7167\u8A2D\u5B9A` \u306B\u79FB\u52D5\u3057\u3001\
  `Microsoft VBScript Regular Expressions\u2026"
lastmod: '2024-04-05T22:37:50.148682-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u3044\u65B9\uFF1A VBA\u3067\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\
  \u3059\u308B\u306B\u306F\u3001\u6700\u521D\u306B Microsoft VBScript Regular Expressions\
  \ \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6709\u52B9\u306B\u3059\u308B\u5FC5\u8981\
  \u304C\u3042\u308A\u307E\u3059\u3002VBA\u30A8\u30C7\u30A3\u30BF\u3067 `\u30C4\u30FC\
  \u30EB` -> `\u53C2\u7167\u8A2D\u5B9A` \u306B\u79FB\u52D5\u3057\u3001`Microsoft VBScript\
  \ Regular Expressions 5.5` \u3092\u30C1\u30A7\u30C3\u30AF\u3057\u307E\u3059\u3002\
  \ \u6587\u5B57\u5217\u5185\u306B\u30D1\u30BF\u30FC\u30F3\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u898B\u3064\u3051\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\
  \uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方：
VBAで正規表現を使用するには、最初に Microsoft VBScript Regular Expressions ライブラリを有効にする必要があります。VBAエディタで `ツール` -> `参照設定` に移動し、`Microsoft VBScript Regular Expressions 5.5` をチェックします。

文字列内にパターンが存在するかを見つける基本的な例です：

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' "is" という単語を探す
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "Pattern found."
    Else
        MsgBox "Pattern not found."
    End If
End Sub
```

文字列でパターンを置換するには：

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' 任意の空白文字に一致
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' 出力: "This_is_a_test_string."
End Sub
```

## ディープダイブ
プログラミング言語での正規表現の導入は、1970年代のUnixツールにまで遡ることがよくあります。VBAはVBScript Regular Expressionsライブラリを通じてregexを組み込み、ExcelやAccessのようにテキスト操作が重要とされないアプリケーションでもテキスト処理タスクの重要性を強調しました。

その力にもかかわらず、VBAでのregexは、PythonやJavaScriptなどのより現代的な言語の実装と比べて直感的でなかったり、パフォーマンスが劣っていることがあります。例えば、Pythonの`re`モジュールは名前付きグループやより洗練されたパターンマッチング機能を広くサポートしており、よりクリーンで読みやすいアプローチを提供します。しかし、VBAエコシステム内で作業している場合、パターンマッチングやテキスト操作を必要とするタスクに対して、正規表現は依然として貴重なツールとして残ります。Officeアプリケーションでの文字列を扱う際に、regexが提供する便利さと機能の面で、効率のトレードオフはしばしば無視できるものです。
