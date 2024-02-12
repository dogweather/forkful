---
title:                "JSONとの作業"
aliases: - /ja/vba/working-with-json.md
date:                  2024-02-01T22:06:12.813415-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

JSON（JavaScript Object Notation）は、人が読み書きしやすく、機械が解析し生成しやすい軽量なデータ交換フォーマットです。プログラマーは、サーバーとWebアプリケーション間でデータを伝送するためや、さまざまなプログラミング環境内で情報を構造化されたアクセスしやすい方法で保存するためにJSONを使用します。これにはVisual Basic for Applications (VBA)も含まれます。

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
