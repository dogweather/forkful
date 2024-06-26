---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:49.316919-07:00
description: "\u65B9\u6CD5\uFF1A VBA\u3067\u306F\u3001`Function`\u304A\u3088\u3073\
  `End Function`\u306E\u30B9\u30C6\u30FC\u30C8\u30E1\u30F3\u30C8\u3092\u4F7F\u7528\
  \u3057\u3066\u95A2\u6570\u3092\u5B9A\u7FA9\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\
  \u3001\u9577\u65B9\u5F62\u306E\u9762\u7A4D\u3092\u8A08\u7B97\u3059\u308B\u95A2\u6570\
  \u3092\u4F5C\u6210\u3059\u308B\u65B9\u6CD5\u306E\u7C21\u5358\u306A\u4F8B\u3067\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:37:50.172077-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A VBA\u3067\u306F\u3001`Function`\u304A\u3088\u3073`End\
  \ Function`\u306E\u30B9\u30C6\u30FC\u30C8\u30E1\u30F3\u30C8\u3092\u4F7F\u7528\u3057\
  \u3066\u95A2\u6570\u3092\u5B9A\u7FA9\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  \u9577\u65B9\u5F62\u306E\u9762\u7A4D\u3092\u8A08\u7B97\u3059\u308B\u95A2\u6570\u3092\
  \u4F5C\u6210\u3059\u308B\u65B9\u6CD5\u306E\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A\
  ."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u7D44\u7E54\u5316\u3059\u308B"
weight: 18
---

## 方法：
VBAでは、`Function`および`End Function`のステートメントを使用して関数を定義します。以下は、長方形の面積を計算する関数を作成する方法の簡単な例です：

```basic
Function CalculateArea(length As Double, width As Double) As Double
    CalculateArea = length * width
End Function
```

この関数をVBAコードで呼び出し、結果をメッセージボックスに表示するには、次のように使用します：

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "面積は " & area
End Sub
```

実行されると、このコードは`面積は 50`と記載されたメッセージボックスを表示します。

### 変数をByRefおよびByValで渡す
VBAでは、変数を関数に参照（`ByRef`）または値（`ByVal`）で渡すことができます。前者は元の変数が関数によって変更される可能性があることを意味し、後者はコピーを渡すことで元の変数を変更から守ります。

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## 詳細な説明
イベント駆動型プログラミング言語としてのVBAは、さまざまなタスクを処理するために関数とサブルーチンに大きな重点を置いています。多くの現代の言語とは異なり、VBAには`Function`キーワードが再利用可能なコードブロックを宣言するだけでなく、関数名に直接割り当てられた暗黙の戻り値も許可するという独特の特徴があります。

歴史的に、VBAの関数の設計は、エンカプセレーションとモジュール性がソフトウェア開発での重要性を徐々に認識されていた以前のプログラミングパラダイムに影響を受けています。この歴史的背景がVBAにコードを編成するためのいくぶん保守的だが機能的なアプローチを採用させています。

VBAはそのネイティブ環境（例えば、Microsoft Officeアプリケーション）内で強力ですが、プログラミングの世界は進化していることを理解することが重要です。Pythonなどの言語はより直接的な構文と広範な標準ライブラリを提供しており、Officeスイート外のさまざまなアプリケーションにとって好ましい代替手段となっています。ただし、Microsoft Office製品内で作業する際は、VBAが提供する統合性と自動化の機能は無類のものです。

それにもかかわらず、VBAの周りのコミュニティは年齢にもかかわらず活動的であり、その機能を活用するための革新的な方法を絶えず探しています。しかし、ソフトウェア業界がより現代的で、汎用性が高く、堅牢な言語へと移行するにつれて、VBAに精通しているプログラマーは、Office関連のタスク以外でこれらの代替言語を探求し、コーディングのツールキットを広げることが奨励されています。
