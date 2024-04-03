---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:47.034464-07:00
description: "\u4F7F\u3044\u65B9\uFF1A \u4ED6\u306E\u5358\u7D14\u306A\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3068\u306F\u7570\u306A\u308A\u3001VBA\u306F\
  \u4E3B\u306BMicrosoft Office\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\
  \u3067\u306E\u57CB\u3081\u8FBC\u307F\u7528\u306B\u8A2D\u8A08\u3055\u308C\u3066\u3044\
  \u308B\u305F\u3081\u3001\u4E00\u822C\u7684\u306A\u610F\u5473\u3067\u76F4\u63A5\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u305F\
  \u3081\u306E\u7D44\u307F\u8FBC\u307F\u6A5F\u80FD\u306F\u3042\u308A\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001\u3061\u3087\u3063\u3068\u3057\u305F\u5275\u9020\u6027\
  \u3092\u4F7F\u3048\u3070\u3001Windows Script\u2026"
lastmod: '2024-03-13T22:44:41.908845-06:00'
model: gpt-4-0125-preview
summary: "\u4ED6\u306E\u5358\u7D14\u306A\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u74B0\u5883\u3068\u306F\u7570\u306A\u308A\u3001VBA\u306F\u4E3B\u306BMicrosoft Office\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u306E\u57CB\u3081\u8FBC\u307F\
  \u7528\u306B\u8A2D\u8A08\u3055\u308C\u3066\u3044\u308B\u305F\u3081\u3001\u4E00\u822C\
  \u7684\u306A\u610F\u5473\u3067\u76F4\u63A5\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  \u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\
  \u6A5F\u80FD\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u3061\
  \u3087\u3063\u3068\u3057\u305F\u5275\u9020\u6027\u3092\u4F7F\u3048\u3070\u3001Windows\
  \ Script Host\uFF08WSH\uFF09\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u5916\u90E8\
  API\u3092\u547C\u3073\u51FA\u3059\u3053\u3068\u3067\u985E\u4F3C\u306E\u6A5F\u80FD\
  \u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306FWSH\u3092\u4F7F\
  \u7528\u3057\u305F\u5B9F\u7528\u7684\u306A\u56DE\u907F\u7B56\u3067\u3059\uFF1A\n\
  \n1."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u8FBC\
  \u307F"
weight: 23
---

## 使い方：
他の単純なプログラミング環境とは異なり、VBAは主にMicrosoft Officeアプリケーション内での埋め込み用に設計されているため、一般的な意味で直接コマンドライン引数を読み取るための組み込み機能はありません。しかし、ちょっとした創造性を使えば、Windows Script Host（WSH）を使用するか、外部APIを呼び出すことで類似の機能を実現できます。以下はWSHを使用した実用的な回避策です：

1. **VBAへ引数を渡すVBScriptを作成する：**

   最初に、VBAアプリケーション（例：Excelマクロ）を起動し、コマンドライン引数を渡すVBScriptファイル（*yourScript.vbs*）を作成します：

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\YourMacroWorkbook.xlsm"
objExcel.Run "YourMacroName", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **VBAで引数にアクセスする：**

   VBAアプリケーション（*YourMacroWorkbook.xlsm*）で、パラメータを受け入れるようにマクロ（*YourMacroName*）を修正または作成します：

```vb
Sub YourMacroName(arg1 As String, arg2 As String)
    MsgBox "Argument 1: " & arg1 & " Argument 2: " & arg2
End Sub
```

3. **スクリプトを実行する：**

   コマンドラインから引数を指定してVBScriptを実行します：

```shell
cscript yourScript.vbs "Hello" "World"
```

   これにより、"Hello" と "World" の引数でVBAマクロが実行され、メッセージボックスにそれらが表示されるはずです。

## 詳細分析：
歴史的な文脈で、VBAはMicrosoft Officeアプリケーションの機能を拡張するために考案されましたが、独立したプログラミング環境としてではありません。そのため、コマンドラインとの直接のやりとりはその主要な範囲外であり、コマンドライン引数を読み取るための組み込みサポートがないことを説明しています。

上述の方法は効果的ですが、外部スクリプティングを利用してギャップを埋めるネイティブソリューションよりも回避策にすぎません。このアプローチは、マクロを有効にする必要があり、実行するためにセキュリティ設定を下げる可能性があるため、複雑さと潜在的なセキュリティ上の懸念を生じさせます。

コマンドライン引数に大きく依存しているタスクや、Windowsオペレーティングシステムとのよりシームレスな統合が必要な場合、PowerShellやPythonのような他のプログラミング言語が、より堅牢で安全なソリューションを提供するかもしれません。これらの代替案はコマンドライン引数を直接サポートしており、外部入力によって動的にその動作を変更する必要があるスタンドアロンアプリケーションやスクリプトに適しています。
