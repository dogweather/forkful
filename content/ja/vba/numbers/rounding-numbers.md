---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:34.620754-07:00
description: "\u65B9\u6CD5: Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306F\
  \u3001\u3044\u304F\u3064\u304B\u306E\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u4E38\
  \u3081\u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002\u305D\u308C\u305E\u308C\u7279\
  \u5B9A\u306E\u30B7\u30CA\u30EA\u30AA\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306F\u3001\u4F8B\u3068\u3068\u3082\u306B\u6700\u3082\u4E00\u822C\u7684\
  \u306B\u4F7F\u7528\u3055\u308C\u308B\u95A2\u6570\u3067\u3059\uFF1A 1. **Round \u95A2\
  \u6570**: `Round` \u95A2\u6570\u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\u6841\u6570\
  \u306B\u6570\u5024\u3092\u4E38\u3081\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.877358-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u306F\u3001\u3044\u304F\
  \u3064\u304B\u306E\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u4E38\u3081\u3092\u5B9F\
  \u73FE\u3067\u304D\u307E\u3059\u3002\u305D\u308C\u305E\u308C\u7279\u5B9A\u306E\u30B7\
  \u30CA\u30EA\u30AA\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\
  \u3001\u4F8B\u3068\u3068\u3082\u306B\u6700\u3082\u4E00\u822C\u7684\u306B\u4F7F\u7528\
  \u3055\u308C\u308B\u95A2\u6570\u3067\u3059\uFF1A\n\n1."
title: "\u6570\u5024\u306E\u56DB\u6368\u4E94\u5165"
weight: 13
---

## 方法:
Visual Basic for Applications（VBA）では、いくつかの関数を使用して丸めを実現できます。それぞれ特定のシナリオに適しています。以下は、例とともに最も一般的に使用される関数です：

1. **Round 関数**:
   `Round` 関数は、指定された桁数に数値を丸めます。
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' 出力: 3.14
   MsgBox roundedNumber
   ```
   
2. **Int と Fix 関数**:
   `Int` と `Fix` の両関数は、最も近い整数に数値を切り下げるために使用されますが、負の数値に対しては異なる動作をします。
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' 出力: -4
   fixRounded = Fix(-3.14159)  ' 出力: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Ceiling と Floor 関数**:
   VBAには他の言語で見られる組み込みの `Ceiling` と `Floor` 関数がありません。これを模倣するには、Excel VBAの `Application.WorksheetFunction.Ceiling_Math` と `Application.WorksheetFunction.Floor_Math` を使用します。
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' 出力: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' 出力: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## 深掘り
VBAの `Round` 関数は、**バンカーの丸め**を使用しているため、他の言語の丸めメソッドとは根本的に異なります。バンカーの丸めは、二つの数値のちょうど中間にある場合に最も近い偶数に丸めるため、大規模なデータセットにおける計算の偏りを減らし、より統計的に有意な結果を提供します。しかし、これはそれに慣れていない人にとって、特に各ケースでの整数精度が期待される場合には、予期せぬ振る舞いにつながる可能性があります。

対照的に、多くのプログラミング言語やシステムは、「算術丸め」または「半分上げ丸め」を使用しており、二つの可能な丸め値のちょうど中間にある数値は常に上に丸められます。他の言語からVBAにコードを変換または移植する場合、プログラマーは金融や統計のアプリケーションにおける微妙なバグや不正確さを避けるため、これらの違いを念頭に置く必要があります。

VBAは丸めに関するさまざまな関数を提供していますが、`Ceiling` と `Floor` 関数がない（ExcelのWorksheetFunctionに頼らずに）ことは、そのネイティブ機能の限界を強調しています。より機能豊富な言語から来たプログラマーは、これらの除外が不便であり、利用可能な関数を使用して計算を適応させるか、カスタムソリューションを実装する必要があるかもしれません。これらの制限にもかかわらず、VBAの丸め関数を正しく理解して使用することで、数値計算が正確であり、ほとんどのアプリケーションの要件を満たすことができます。
