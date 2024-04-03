---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:47.034464-07:00
description: "Visual Basic for\u2026"
lastmod: '2024-03-13T22:44:41.908845-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u30B3\u30DE\u30F3\u30C9\
  \u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u3068\u3044\u3046\u306E\
  \u306F\u3001\u5B9F\u884C\u6642\u306B\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u6E21\u3055\
  \u308C\u305F\u30D1\u30E9\u30E1\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u306E\u6280\u8853\u306F\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306E\u4ECB\u5165\u306A\u3057\u306B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u52D5\u4F5C\u3084\u51FA\u529B\u3092\u5F71\u97FF\u3092\u4E0E\u3048\
  \u308B\u305F\u3081\u306B\u3088\u304F\u4F7F\u7528\u3055\u308C\u3001\u81EA\u52D5\u5316\
  \u3084\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\u30F3\u30B0\u306E\u30BF\u30B9\u30AF\u3092\
  \u5927\u5E45\u306B\u30B7\u30F3\u30D7\u30EB\u304B\u3064\u591A\u7528\u9014\u306B\u3057\
  \u307E\u3059\u3002."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u8FBC\
  \u307F"
weight: 23
---

## 何となぜ？

Visual Basic for Applications（VBA）でコマンドライン引数を読み取るというのは、実行時にプログラムに渡されたパラメータにアクセスすることを意味します。この技術は、ユーザーの介入なしにプログラムの動作や出力を影響を与えるためによく使用され、自動化やスクリプティングのタスクを大幅にシンプルかつ多用途にします。

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
