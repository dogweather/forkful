---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:29.259846-07:00
description: "\u65B9\u6CD5\uFF1A Visual Basic for Applications (VBA)\u3067\u306F\u3001\
  `LCase`\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u3053\
  \u306E\u95A2\u6570\u306F\u6587\u5B57\u5217\u3092\u5165\u529B\u3068\u3057\u3066\u53D7\
  \u3051\u53D6\u308A\u3001\u3059\u3079\u3066\u306E\u5927\u6587\u5B57\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3057\u305F\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u8FD4\
  \u3057\u307E\u3059\u3002\u3053\u306E\u57FA\u672C\u7684\u306A\u4F8B\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-04-05T21:53:42.761986-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B"
weight: 4
---

## 方法：
Visual Basic for Applications (VBA)では、`LCase`関数を使用して文字列を小文字に変換するのは簡単です。この関数は文字列を入力として受け取り、すべての大文字を小文字に変換した新しい文字列を返します。この基本的な例を見てみましょう：

```basic
Dim originalString As String
Dim lowerCaseString As String

originalString = "Hello, World!"
lowerCaseString = LCase(originalString)

Debug.Print lowerCaseString ' 出力: hello, world!
```

比較や代入で`LCase`を直接使用することも、コードを簡潔にするために可能です：

```basic
If LCase(userInput) = "yes" Then
    Debug.Print "User said yes"
End If
```

この二つ目の例は、入力を比較する前に小文字に変換することによって、ユーザー入力を大文字と小文字を区別しない方法で扱う方法を示しています。

## 詳細
`LCase`関数はVBAでの文字列操作の基盤となっており、言語の誕生以来のコア機能でした。`LCase`は、データ解析やユーザー入力処理のシナリオで一般的なケース変換タスクを簡素化します。`LCase`は様々なアプリケーションで小文字への文字変換のニーズに効果的に対応しながら、その制限と代替案を認識することも重要です。

例えば、`LCase`は英字アルファベットにはスムーズに機能しますが、より複雑な大文字と小文字のルールを持つ言語を扱う場合は、ケース変換のために適切なロケール設定で`StrConv`関数を使用することが追加で考慮される必要があります。

さらに、`str.lower()`を使用するPythonや、`string.toLowerCase()`を使用するJavaScriptのような言語から移行する際、プログラマーは`LCase`を直感的に見つけるかもしれませんが、メソッドのチェーンがないなど、VBAの特有の仕様を心に留めておく必要があります。

まとめると、他の言語では新しくてより強力な代替手段が存在するかもしれませんが、`LCase`はVBAで文字列を小文字に変換するための信頼できるシンプルな関数として残り、その全体の構文と機能のスキーマにうまく収まっています。
