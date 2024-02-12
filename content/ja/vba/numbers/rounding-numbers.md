---
title:                "数値の四捨五入"
aliases:
- /ja/vba/rounding-numbers.md
date:                  2024-02-01T22:01:34.620754-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の四捨五入"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおいて数字の丸めは、数値を最も近い整数や特定の小数点以下の桁数に近似することについてです。プログラマーは数字を丸めることで、数字を簡素化し、可読性を向上させるか、計算において特定の数値基準を満たすために使用します。これは、特に精度が重要視される金融計算において特にそうです。

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
