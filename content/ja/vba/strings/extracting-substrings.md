---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:18.673221-07:00
description: "\u65B9\u6CD5\uFF1A VBA\u3067\u306F\u3001\u4E3B\u306B`Mid`\u3001`Left`\u3001\
  `Right`\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\
  \u30B0\u3092\u62BD\u51FA\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u3001\u3053\u308C\
  \u3089\u306E\u95A2\u6570\u3092\u4F8B\u3068\u3068\u3082\u306B\u63A2\u308A\u307E\u3059\
  \uFF1A 1. **Mid**: \u6307\u5B9A\u3057\u305F\u4F4D\u7F6E\u304B\u3089\u59CB\u307E\u308B\
  \u6587\u5B57\u5217\u304B\u3089\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u62BD\
  \u51FA\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.867969-06:00'
model: gpt-4-0125-preview
summary: "VBA\u3067\u306F\u3001\u4E3B\u306B`Mid`\u3001`Left`\u3001`Right`\u95A2\u6570\
  \u3092\u4F7F\u7528\u3057\u3066\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u62BD\
  \u51FA\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u3001\u3053\u308C\u3089\u306E\u95A2\
  \u6570\u3092\u4F8B\u3068\u3068\u3082\u306B\u63A2\u308A\u307E\u3059\uFF1A\n\n1."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## 方法：
VBAでは、主に`Mid`、`Left`、`Right`関数を使用してサブストリングを抽出します。以下に、これらの関数を例とともに探ります：

1. **Mid**: 指定した位置から始まる文字列からサブストリングを抽出します。
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Mid(exampleString, 7, 5)
   Debug.Print result  ' 出力: World
   ```

2. **Left**: 文字列の左から、指定された文字数までのサブストリングを抽出します。
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Left(exampleString, 5)
   Debug.Print result  ' 出力: Hello
   ```

3. **Right**: 文字列の右から、指定された文字数までのサブストリングを抽出します。
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Right(exampleString, 5)
   Debug.Print result  ' 出力: World
   ```

これらの基本関数は、VBAにおいてサブストリング抽出の基盤を形成し、文字列操作に対する堅牢で直接的なアプローチを提供します。

## 深堀り：
歴史的に、プログラミングにおいて文字列を操作する能力は本質的であり、BASIC（VBAの先駆者）は個人コンピューティングの初期段階でこの能力を民主化した初めてのものの一つでした。VBAの`Mid`、`Left`、および`Right`関数はこの遺産を受け継ぎ、現代のプログラマー向けに簡素化されたインタフェースを提供します。

これらの関数は多くのタスクに非常に効果的ですが、新しい言語での正規表現の出現は、テキスト作業により強力で柔軟な方法を提供しました。それにもかかわらず、従来のVBAサブストリング関数の即時的な単純さと利用可能性は、素早いタスクやプログラミング初心者にとって完全に適しています。

文字列内のより複雑な解析および検索操作については、VBAは`Like`オペレータや正規表現を`VBScript.RegExp`オブジェクトを介してサポートしていますが、これらは効果的に使用するために少し設定と理解が必要です。これらのツールはより大きな力を提供しますが、`Mid`、`Left`、および`Right`の直接的な性質は、多くのVBAプログラムでのそれらの継続的な関連性と有用性を保証します。
