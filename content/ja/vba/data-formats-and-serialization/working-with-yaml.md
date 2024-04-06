---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:31.153136-07:00
description: null
lastmod: '2024-04-05T21:53:42.813387-06:00'
model: gpt-4-0125-preview
summary: "VBA\u3067YAML\u3092\u6271\u3046\u306B\u306F\u3001YAML\u3092VBA\u304C\u5BB9\
  \u6613\u306B\u64CD\u4F5C\u3067\u304D\u308B\u5F62\u5F0F\u3001\u901A\u5E38\u306F\u8F9E\
  \u66F8\u3084\u30B3\u30EC\u30AF\u30B7\u30E7\u30F3\u306B\u89E3\u6790\u304A\u3088\u3073\
  \u5909\u63DB\u3059\u308B\u65B9\u6CD5\u3092\u7406\u89E3\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059\u3002\u6B8B\u5FF5\u306A\u304C\u3089\u3001VBA\u306FYAML\u306E\
  \u89E3\u6790\u3084\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3092\
  \u30CD\u30A4\u30C6\u30A3\u30D6\u306B\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\
  \u305B\u3093\u3002\u3057\u304B\u3057\u3001YAML\u304CJSON\u3068\u5BC6\u63A5\u306B\
  \u95A2\u9023\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u8003\u616E\u3057\u3066\u3001\
  JSON\u5909\u63DB\u30C4\u30FC\u30EB\u3068\u8F9E\u66F8\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u306E\u7D44\u307F\u5408\u308F\u305B\u3092\u4F7F\u7528\u3057\u3066YAML\u30C7\
  \u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "YAML\u3068\u306E\u4F5C\u696D"
weight: 41
---

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
