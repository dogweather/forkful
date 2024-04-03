---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:12.813415-07:00
description: "\u65B9\u6CD5: VBA\u306F\u3001\u30CD\u30A4\u30C6\u30A3\u30D6\u306BJSON\u306E\
  \u89E3\u6790\u3084\u751F\u6210\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\
  \u305B\u3093\u306E\u3067\u3001\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\u30F3\u30B0\u8A00\
  \u8A9E\u3067\u3042\u308BJScript\uFF08ScriptControl\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u3092\u4ECB\u3057\u3066\uFF09\u3092\u4F7F\u7528\u3057\u3066JSON\u6587\u5B57\
  \u5217\u3092\u89E3\u6790\u3057\u3001JSON\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\
  \u69CB\u7BC9\u3057\u307E\u3059\u3002\u3053\u308C\u304C\u3001VBA\u3067JSON\u6587\u5B57\
  \u5217\u3092\u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059."
lastmod: '2024-03-13T22:44:41.915971-06:00'
model: gpt-4-0125-preview
summary: "VBA\u306F\u3001\u30CD\u30A4\u30C6\u30A3\u30D6\u306BJSON\u306E\u89E3\u6790\
  \u3084\u751F\u6210\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\
  \u306E\u3067\u3001\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\u30F3\u30B0\u8A00\u8A9E\u3067\
  \u3042\u308BJScript\uFF08ScriptControl\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\
  \u4ECB\u3057\u3066\uFF09\u3092\u4F7F\u7528\u3057\u3066JSON\u6587\u5B57\u5217\u3092\
  \u89E3\u6790\u3057\u3001JSON\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u69CB\u7BC9\
  \u3057\u307E\u3059\u3002\u3053\u308C\u304C\u3001VBA\u3067JSON\u6587\u5B57\u5217\u3092\
  \u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059."
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
