---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:12.813415-07:00
description: "JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.915971-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object Notation\uFF09\u306F\u3001\u4EBA\u304C\u8AAD\
  \u307F\u66F8\u304D\u3057\u3084\u3059\u304F\u3001\u6A5F\u68B0\u304C\u89E3\u6790\u3057\
  \u751F\u6210\u3057\u3084\u3059\u3044\u8EFD\u91CF\u306A\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u3068Web\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u9593\u3067\u30C7\u30FC\u30BF\u3092\u4F1D\u9001\u3059\u308B\u305F\
  \u3081\u3084\u3001\u3055\u307E\u3056\u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\
  \u30B0\u74B0\u5883\u5185\u3067\u60C5\u5831\u3092\u69CB\u9020\u5316\u3055\u308C\u305F\
  \u30A2\u30AF\u30BB\u30B9\u3057\u3084\u3059\u3044\u65B9\u6CD5\u3067\u4FDD\u5B58\u3059\
  \u308B\u305F\u3081\u306BJSON\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306B\u306FVisual Basic for Applications (VBA)\u3082\u542B\u307E\u308C\u307E\u3059\
  \u3002."
title: "JSON\u3068\u306E\u4F5C\u696D"
weight: 38
---

## 方法:
VBAは、ネイティブにJSONの解析や生成をサポートしていませんので、スクリプティング言語であるJScript（ScriptControlオブジェクトを介して）を使用してJSON文字列を解析し、JSONオブジェクトを構築します。これが、VBAでJSON文字列を解析する方法です:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Name: " & parsed.name & ", Age: " & parsed.age & ", City: " & parsed.city
End Sub
```

JSONを生成するには、同様の方法を使用し、連結を通じてJSON文字列を構築します:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## より深く
紹介された方法は、ScriptControlを利用してJSONを扱うもので、基本的にはJavaScriptエンジンに作業を外注するものです。これは創造的な回避策ですが、VBAのコンテキストでJSONを扱う上で最も効率的であるとか、最も現代的な方法であるとは限りません。より複雑なアプリケーションでこの方法を使うと、性能のオーバーヘッドやセキュリティ上の懸念を引き起こす可能性があります。なぜなら、ScriptControlはホストコンピューターに完全なアクセス権を持つ環境で実行されるからです。

PythonやJavaScriptのような他のプログラミング環境は、JSONをサポートしているため、広範なJSON操作が必要なアプリケーションにはより適しています。これらの言語は、解析や生成だけでなく、JSONデータのクエリやフォーマットも容易にする包括的なライブラリを提供します。

VBAにおけるこれらの限界にもかかわらず、Webベースのデータ交換や設定ファイルが主にJSON形式である世界で、JSONを扱う方法を理解することは重要です。VBAプログラマーにとって、これらの技術をマスターすることは、Web APIとの統合や設定ファイルの解釈、あるいは簡単なWebアプリケーションの構築への道を開きます。しかし、プロジェクトが複雑さを増すか、高性能を要求する場合、開発者はよりJSONに優しいプログラミング環境を活用することを検討するかもしれません。
