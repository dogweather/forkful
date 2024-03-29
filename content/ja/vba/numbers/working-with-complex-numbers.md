---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:16.922177-07:00
description: "\u8907\u7D20\u6570\u3092\u6271\u3046\u4F5C\u696D\u306F\u3001\u5B9F\u90E8\
  \u3068\u865A\u90E8\u306E\u4E21\u65B9\u3092\u6301\u3064\u6570\u306B\u5BFE\u3057\u3066\
  \u6570\u5B66\u7684\u6F14\u7B97\u3092\u5B9F\u884C\u3059\u308B\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u305D\u3057\u3066\
  \u5B9F\u6570\u3060\u3051\u3067\u306F\u89E3\u6C7A\u3067\u304D\u306A\u3044\u65B9\u7A0B\
  \u5F0F\u3092\u89E3\u304F\u3053\u3068\u304C\u542B\u307E\u308C\u308B\u3069\u3093\u306A\
  \u9818\u57DF\u3067\u3082\u3001\u3057\u3070\u3057\u3070\u8907\u7D20\u6570\u3092\u6271\
  \u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.875122-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u3092\u6271\u3046\u4F5C\u696D\u306F\u3001\u5B9F\u90E8\
  \u3068\u865A\u90E8\u306E\u4E21\u65B9\u3092\u6301\u3064\u6570\u306B\u5BFE\u3057\u3066\
  \u6570\u5B66\u7684\u6F14\u7B97\u3092\u5B9F\u884C\u3059\u308B\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A8\u30F3\
  \u30B8\u30CB\u30A2\u30EA\u30F3\u30B0\u3001\u7269\u7406\u5B66\u3001\u305D\u3057\u3066\
  \u5B9F\u6570\u3060\u3051\u3067\u306F\u89E3\u6C7A\u3067\u304D\u306A\u3044\u65B9\u7A0B\
  \u5F0F\u3092\u89E3\u304F\u3053\u3068\u304C\u542B\u307E\u308C\u308B\u3069\u3093\u306A\
  \u9818\u57DF\u3067\u3082\u3001\u3057\u3070\u3057\u3070\u8907\u7D20\u6570\u3092\u6271\
  \u3044\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

複素数を扱う作業は、実部と虚部の両方を持つ数に対して数学的演算を実行することを含みます。プログラマーは、エンジニアリング、物理学、そして実数だけでは解決できない方程式を解くことが含まれるどんな領域でも、しばしば複素数を扱います。

## 方法:

Visual Basic for Applications（VBA）での複素数の扱いは、ネイティブサポートを提供する言語と比較して、いくぶん直感的ではありません。しかし、関数を作成するか、既存のライブラリ関数を使用することで、複雑な演算を管理することができます。複素数の加算、減算、乗算、除算の基本的な例を探りましょう：

```vb
' 複素数を加算する関数
Function AddComplex(x As String, y As String) As String
    Dim real1 As Double, imag1 As Double
    Dim real2 As Double, imag2 As Double
    
    ' 複素数から実部と虚部を抽出
    real1 = Val(Split(x, "+")(0))
    imag1 = Val(Split(x, "+")(1))
    real2 = Val(Split(y, "+")(0))
    imag2 = Val(Split(y, "+")(1))
    
    ' 加算を実行
    AddComplex = (real1 + real2) & "+" & (imag1 + imag2) & "i"
End Function

' 例の使用
Sub ExampleUsage()
    Dim result As String
    result = AddComplex("3+2i", "1+7i")
    Debug.Print "加算の結果: " & result  ' 出力: 加算の結果: 4+9i
End Sub
```

この例は加算を示していますが、減算、乗算、除算に類似のアプローチが適用可です。基本的な算術を超える複雑な操作の場合、外部ライブラリを探索するか、複素数操作をよりネイティブにサポートする他のソリューションを統合する価値があるかもしれません。

## 深い考察:

VBAには複素数のための組み込みサポートが含まれておらず、複素数クラス(`complex`)を持つPythonやStandard Template Library(`std::complex`)を有するC++などの言語と比較して立ち後れている側面があります。伝統的に、VBAで直接複素数を操作する必要性は比較的まれで、主にオフィスアプリケーションの操作、自動化、そして従来複雑な数学計算を必要としないタスクに使用されます。VBAが考案され、開発された当初、そのユースケースは主にビジネスアプリケーションに焦点を当てており、科学計算よりも、それが欠落している理由を説明することができます。

複雑な数の操作を広範囲にわたって必要とするタスクでは、より数学的に指向された言語の使用が有益であるかもしれません。しかし、VBAの使用にコミットしているか、それに制限されている人々にとっては、カスタム関数の作成（例で示されているように）やこれらの能力を持つソフトウェア（例えばMATLABやある程度Excel自体）との統合は、前進するための実現可能な道です。その限界にもかかわらず、創造的な解決策と外部統合は、VBAのユーティリティをそれが元々設計されていなかった領域、複素数を扱うことを含むものへと拡張することができます。
