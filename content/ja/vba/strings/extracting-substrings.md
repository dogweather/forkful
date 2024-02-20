---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:18.673221-07:00
description: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306E\u30B5\u30D6\
  \u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u306F\u3001\u6307\u5B9A\u3055\u308C\
  \u305F\u57FA\u6E96\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\u5217\u306E\u7279\u5B9A\
  \u306E\u90E8\u5206\u3092\u5206\u96E2\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30AD\u30B9\u30C8\
  \u30C7\u30FC\u30BF\u304B\u3089\u60C5\u5831\u3092\u64CD\u4F5C\u304A\u3088\u3073\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u304C\u91CD\u8981\u306A\u30BF\u30B9\u30AF\u3001\u4F8B\
  \u3048\u3070\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u691C\u8A3C\u3001\u304A\u3088\u3073\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u8A2D\u5B9A\u306A\u3069\u3067\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.036518
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306E\u30B5\u30D6\u30B9\
  \u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA\u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\
  \u57FA\u6E96\u306B\u57FA\u3065\u3044\u3066\u6587\u5B57\u5217\u306E\u7279\u5B9A\u306E\
  \u90E8\u5206\u3092\u5206\u96E2\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\
  \u30FC\u30BF\u304B\u3089\u60C5\u5831\u3092\u64CD\u4F5C\u304A\u3088\u3073\u62BD\u51FA\
  \u3059\u308B\u3053\u3068\u304C\u91CD\u8981\u306A\u30BF\u30B9\u30AF\u3001\u4F8B\u3048\
  \u3070\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u691C\u8A3C\u3001\u304A\u3088\u3073\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u8A2D\u5B9A\u306A\u3069\u3067\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications（VBA）でのサブストリングの抽出は、指定された基準に基づいて文字列の特定の部分を分離することを含みます。プログラマーは、テキストデータから情報を操作および抽出することが重要なタスク、例えばデータ解析、検証、およびフォーマット設定などでこれを行います。

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
