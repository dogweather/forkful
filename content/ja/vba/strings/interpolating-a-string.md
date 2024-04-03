---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:51.371803-07:00
description: "\u65B9\u6CD5 \u4E00\u90E8\u306E\u8A00\u8A9E\u306B\u306F\u7D44\u307F\u8FBC\
  \u307F\u306E\u6587\u5B57\u5217\u88DC\u9593\u304C\u5B58\u5728\u3059\u308B\u4E00\u65B9\
  \u3067\u3001VBA\u3067\u306F\u901A\u5E38 `&` \u6F14\u7B97\u5B50\u3084 `Format` \u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u5909\u6570\u3092\u6587\u5B57\u5217\u306B\u57CB\
  \u3081\u8FBC\u3080\u3068\u3044\u3046\u3088\u308A\u624B\u52D5\u306E\u30A2\u30D7\u30ED\
  \u30FC\u30C1\u304C\u5FC5\u8981\u3067\u3059\u3002\u4EE5\u4E0B\u306B\u3053\u308C\u3089\
  \u306E\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3092\u6319\u3052\u307E\u3059\uFF1A **`&`\
  \ \u6F14\u7B97\u5B50\u3092\u4F7F\u7528\u3059\u308B:**."
lastmod: '2024-03-13T22:44:41.863312-06:00'
model: gpt-4-0125-preview
summary: "\u4E00\u90E8\u306E\u8A00\u8A9E\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\
  \u6587\u5B57\u5217\u88DC\u9593\u304C\u5B58\u5728\u3059\u308B\u4E00\u65B9\u3067\u3001\
  VBA\u3067\u306F\u901A\u5E38 `&` \u6F14\u7B97\u5B50\u3084 `Format` \u95A2\u6570\u3092\
  \u4F7F\u7528\u3057\u3066\u5909\u6570\u3092\u6587\u5B57\u5217\u306B\u57CB\u3081\u8FBC\
  \u3080\u3068\u3044\u3046\u3088\u308A\u624B\u52D5\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\
  \u304C\u5FC5\u8981\u3067\u3059\u3002\u4EE5\u4E0B\u306B\u3053\u308C\u3089\u306E\u65B9\
  \u6CD5\u3092\u793A\u3059\u4F8B\u3092\u6319\u3052\u307E\u3059\uFF1A\n\n**`&` \u6F14\
  \u7B97\u5B50\u3092\u4F7F\u7528\u3059\u308B:**."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## 方法
一部の言語には組み込みの文字列補間が存在する一方で、VBAでは通常 `&` 演算子や `Format` 関数を使用して変数を文字列に埋め込むというより手動のアプローチが必要です。以下にこれらの方法を示す例を挙げます：

**`&` 演算子を使用する:**

```vb
Dim userName As String
Dim userScore As Integer

userName = "Alice"
userScore = 95

' 文字列と変数の連結
Dim message As String
message = "おめでとう, " & userName & "! あなたのスコアは " & userScore & "です。"
Debug.Print message
```
**出力:**
```
おめでとう, Alice! あなたのスコアは 95です。
```

**`Format` 関数を使用する:**

特にフォーマットされた数字や日付を含めるなど、もっと複雑なシナリオには `Format` 関数が非常に重要です。

```vb
Dim currentDate As Date
currentDate = Date

Dim formattedMessage As String
formattedMessage = "今日は " & Format(currentDate, "MMMM dd, yyyy") & "です。素晴らしい一日を！"
Debug.Print formattedMessage
```

**出力:**
```
今日は 2023年4月15日です。素晴らしい一日を！
```

## 深掘り
PythonやJavaScriptのような現代のプログラミング言語で知られている文字列補間は、VBAには直接存在しません。歴史的に、VBAの開発者は `&` を使用した連結や複雑な文字列や正確なフォーマットが必要な場合に `Format` 関数を利用して値を文字列に挿入する必要がありました。この違いはVBAの起源の時代と、一部の現代の便利さよりも直接的なシンプルさに重点を置いていることを強調しています。

しかし、VBAが組み込みの文字列補間を提供しないとしても、シンプルな連結のための `&` やより複雑なシナリオのための `Format` の習得により、堅牢で柔軟な文字列操作が可能になることは重要です。ネイティブに文字列補間機能を持つ言語から来た開発者にとって、これは初めに一歩退くように見えるかもしれませんが、これらの方法は、一度習得すると、非常に強力な制御レベルを提供します。さらに、より最近の.NET環境に移行すると、プログラマーはVB.NETで文字列補間を第一級の機能として見つけ、動的な文字列を作成するためのより馴染み深く効率的なアプローチを提供します。実際には、VBAの違いと制限を理解することが、効率的で読みやすいコードを書き、必要に応じてより現代的なVisual Basic環境への移行を容易にするのに大いに役立ちます。
