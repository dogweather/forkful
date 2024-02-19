---
aliases:
- /ja/vba/working-with-yaml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:31.153136-07:00
description: "YAML\uFF08\"YAML Ain't Markup Language\"\u306E\u7565\uFF09\u306F\u3001\
  \u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\
  \u308C\u308B\u4EBA\u9593\u304C\u8AAD\u307F\u53D6\u308A\u53EF\u80FD\u306A\u30C7\u30FC\
  \u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u8A00\u8A9E\u3067\
  \u3059\u3002\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\
  \u3055\u304B\u3089\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u591A\u304F\u306E\
  \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u306B\u304A\u3044\u3066\u3001\
  \u7279\u306BVisual Basic for\u2026"
lastmod: 2024-02-18 23:08:54.780555
model: gpt-4-0125-preview
summary: "YAML\uFF08\"YAML Ain't Markup Language\"\u306E\u7565\uFF09\u306F\u3001\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\
  \u308B\u4EBA\u9593\u304C\u8AAD\u307F\u53D6\u308A\u53EF\u80FD\u306A\u30C7\u30FC\u30BF\
  \u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u8A00\u8A9E\u3067\u3059\
  \u3002\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\u3055\
  \u304B\u3089\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u591A\u304F\u306E\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u306B\u304A\u3044\u3066\u3001\u7279\
  \u306BVisual Basic for\u2026"
title: "YAML\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

YAML（"YAML Ain't Markup Language"の略）は、設定ファイルに一般的に使用される人間が読み取り可能なデータシリアライゼーション言語です。そのシンプルさと読みやすさから、プログラマーは多くのプログラミング環境において、特にVisual Basic for Applications（VBA）のスクリプティング領域での相互運用性、データの保存と交換を強化するためにしばしばYAMLを使用しています。

## 方法:

VBAでYAMLを扱うには、YAMLをVBAが容易に操作できる形式、通常は辞書やコレクションに解析および変換する方法を理解する必要があります。残念ながら、VBAはYAMLの解析やシリアライゼーションをネイティブにサポートしていません。しかし、YAMLがJSONと密接に関連していることを考慮して、JSON変換ツールと辞書オブジェクトの組み合わせを使用してYAMLデータを扱うことができます。

まず、オンラインコンバーターや開発環境内のYAMLからJSONへの変換ツールを使用して、YAMLデータをJSONに変換します。変換されたら、次の例を使用してVBAでJSONを解析できます。このアプローチは間接的にYAMLを扱うことを可能にします：

```vb
' 辞書用に Microsoft Scripting Runtime を参照追加
' JSON解析用に Microsoft XML, v6.0 を参照追加

Sub ParseYAMLAsJSON()
    Dim jsonText As String
    jsonText = "{""name"": ""John Doe"", ""age"": 30}" ' これはYAMLから変換されたJSONです
    
    ' JSONパーサー関数があると仮定
    Dim parsedData As Dictionary
    Set parsedData = JsonParser(jsonText)
    
    Debug.Print "Name: " & parsedData("name")
    Debug.Print "Age: " & parsedData("age")
End Sub

Function JsonParser(ByVal jsonText As String) As Dictionary
    ' JSON解析ロジックのプレースホルダー - ここでは外部ライブラリを使用するかもしれません
    Set JsonParser = New Dictionary
    JsonParser.Add "name", "John Doe"
    JsonParser.Add "age", 30
End Function
```
この例では、`JsonParser` 関数は、JSONを解析する場所のプレースホルダーとして使用されます。JSON解析を助けるさまざまなライブラリが利用可能で、VBA用の直接YAML解析ライブラリはほとんどありません。

## 深堀り

VBAで直接YAMLを扱えないのは、その年齢と、最初に設計された環境（現代のデータシリアライゼーションフォーマットを意識していなかった）に起因しています。YAML自体は、よりヒューマンフレンドリーな設定ファイルを必要とするアプリケーションの出現とともに、2000年代初頭に人気のある構成およびシリアライゼーションフォーマットとして登場しました。

プログラマーは通常、外部ツールやライブラリを利用してVBAとYAMLの間のギャップを埋めます。これは、さまざまなライブラリを通じて利用できるJSONサポートと、構造と目的の観点からJSONとYAMLの間の類似性があるため、示されているようにYAMLをJSONに変換することを含みます。

VBAで直接YAMLを扱うことは言語の柔軟性を示していますが、YAMLに対するよりネイティブでシームレスなサポートを提供する他のプログラミング環境（例えば、PythonやJavaScript）があることに注意することが重要です。これらの代替手段は、設定やデータシリアライゼーションにYAMLを大量に依存するプロジェクトにはより適しているかもしれません。それでも、VBAを使用することが決まっているか、必要としている人にとっては、JSON変換を通じた間接的な方法は、YAMLデータを管理し操作するための実用的で役立つアプローチのままです。
